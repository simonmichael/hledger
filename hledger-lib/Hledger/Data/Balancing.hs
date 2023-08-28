{-|
Functions for ensuring transactions and journals are balanced.
-}

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Hledger.Data.Balancing
( -- * BalancingOpts
  BalancingOpts(..)
, HasBalancingOpts(..)
, defbalancingopts
  -- * transaction balancing
, isTransactionBalanced
, balanceTransaction
, balanceTransactionHelper
  -- * journal balancing
, journalBalanceTransactions
, journalCheckBalanceAssertions
  -- * tests
, tests_Balancing
)
where

import Control.Monad (forM, forM_, when, unless)
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import "extra" Control.Monad.Extra (whenM)
import Control.Monad.Reader as R (ReaderT, reader, runReaderT, ask, asks)
import Control.Monad.ST (ST, runST)
import Control.Monad.Trans.Class (lift)
import Data.Array.ST (STArray, getElems, newListArray, writeArray)
import Data.Foldable (asum)
import Data.Function ((&))
import Data.Functor ((<&>))
import "base-compat" Data.Functor.Compat (void)
import qualified Data.HashTable.Class as H (toList)
import qualified Data.HashTable.ST.Cuckoo as H
import Data.List (partition, sortOn)
import Data.List.Extra (nubSort)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing, mapMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import qualified Data.Map as M
import Safe (headDef)
import Text.Printf (printf)

import Hledger.Utils
import Hledger.Data.Types
import Hledger.Data.AccountName (isAccountNamePrefixOf)
import Hledger.Data.Amount
import Hledger.Data.Journal
import Hledger.Data.Posting
import Hledger.Data.Transaction
import Hledger.Data.Errors


data BalancingOpts = BalancingOpts
  { ignore_assertions_     :: Bool  -- ^ should failing balance assertions be ignored ?
  , infer_balancing_costs_ :: Bool  -- ^ Are we permitted to infer missing costs to balance transactions ?
                                    --   Distinct from InputOpts{infer_costs_}.
  , commodity_styles_      :: Maybe (M.Map CommoditySymbol AmountStyle)  -- ^ commodity display styles
  } deriving (Show)

defbalancingopts :: BalancingOpts
defbalancingopts = BalancingOpts
  { ignore_assertions_     = False
  , infer_balancing_costs_ = True
  , commodity_styles_      = Nothing
  }

-- | Check that this transaction would appear balanced to a human when displayed.
-- On success, returns the empty list, otherwise one or more error messages.
--
-- In more detail:
-- For the real postings, and separately for the balanced virtual postings:
--
-- 1. Convert amounts to cost where possible
--
-- 2. When there are two or more non-zero amounts
--    (appearing non-zero when displayed, using the given display styles if provided),
--    are they a mix of positives and negatives ?
--    This is checked separately to give a clearer error message.
--    (Best effort; could be confused by postings with multicommodity amounts.)
--
-- 3. Does the amounts' sum appear non-zero when displayed ?
--    (using the given display styles if provided)
--
transactionCheckBalanced :: BalancingOpts -> Transaction -> [String]
transactionCheckBalanced BalancingOpts{commodity_styles_} t = errs
  where
    (rps, bvps) = foldr partitionPosting ([], []) $ tpostings t
      where
        partitionPosting p ~(l, r) = case ptype p of
            RegularPosting         -> (p:l, r)
            BalancedVirtualPosting -> (l, p:r)
            VirtualPosting         -> (l, r)

    -- check for mixed signs, detecting nonzeros at display precision
    setstyles = maybe id mixedAmountSetStyles commodity_styles_
    postingBalancingAmount p
      | "_price-matched" `elem` map fst (ptags p) = mixedAmountStripPrices $ pamount p
      | otherwise                                 = mixedAmountCost $ pamount p
    signsOk ps =
      case filter (not.mixedAmountLooksZero) $ map (setstyles.postingBalancingAmount) ps of
        nonzeros | length nonzeros >= 2
                   -> length (nubSort $ mapMaybe isNegativeMixedAmount nonzeros) > 1
        _          -> True
    (rsignsok, bvsignsok)       = (signsOk rps, signsOk bvps)

    -- check for zero sum, at display precision
    (rsumcost, bvsumcost)       = (foldMap postingBalancingAmount rps, foldMap postingBalancingAmount bvps)
    (rsumdisplay, bvsumdisplay) = (setstyles rsumcost, setstyles bvsumcost)
    (rsumok, bvsumok)           = (mixedAmountLooksZero rsumdisplay, mixedAmountLooksZero bvsumdisplay)

    -- generate error messages, showing amounts with their original precision
    errs = filter (not.null) [rmsg, bvmsg]
      where
        rmsg
          | rsumok        = ""
          | not rsignsok  = "The real postings all have the same sign. Consider negating some of them."
          | otherwise     = "The real postings' sum should be 0 but is: " ++ showMixedAmountOneLineWithoutPrice False rsumcost
        bvmsg
          | bvsumok       = ""
          | not bvsignsok = "The balanced virtual postings all have the same sign. Consider negating some of them."
          | otherwise     = "The balanced virtual postings' sum should be 0 but is: " ++ showMixedAmountOneLineWithoutPrice False bvsumcost

-- | Legacy form of transactionCheckBalanced.
isTransactionBalanced :: BalancingOpts -> Transaction -> Bool
isTransactionBalanced bopts = null . transactionCheckBalanced bopts

-- | Balance this transaction, ensuring that its postings
-- (and its balanced virtual postings) sum to 0,
-- by inferring a missing amount or conversion price(s) if needed.
-- Or if balancing is not possible, because the amounts don't sum to 0 or
-- because there's more than one missing amount, return an error message.
--
-- Transactions with balance assignments can have more than one
-- missing amount; to balance those you should use the more powerful
-- journalBalanceTransactions.
--
-- The "sum to 0" test is done using commodity display precisions,
-- if provided, so that the result agrees with the numbers users can see.
--
balanceTransaction ::
     BalancingOpts
  -> Transaction
  -> Either String Transaction
balanceTransaction bopts = fmap fst . balanceTransactionHelper bopts

-- | Helper used by balanceTransaction and balanceTransactionWithBalanceAssignmentAndCheckAssertionsB;
-- use one of those instead.
-- It also returns a list of accounts and amounts that were inferred.
balanceTransactionHelper ::
     BalancingOpts
  -> Transaction
  -> Either String (Transaction, [(AccountName, MixedAmount)])
balanceTransactionHelper bopts t = do
  (t', inferredamtsandaccts) <-
    transactionInferBalancingAmount (fromMaybe M.empty $ commodity_styles_ bopts) $
    (if infer_balancing_costs_ bopts then transactionInferBalancingCosts else id)
    t
  case transactionCheckBalanced bopts t' of
    []   -> Right (txnTieKnot t', inferredamtsandaccts)
    errs -> Left $ transactionBalanceError t' errs'
      where
        ismulticommodity = (length $ transactionCommodities t') > 1
        errs' =
          [ "Automatic commodity conversion is not enabled."
          | ismulticommodity && not (infer_balancing_costs_ bopts)
          ] ++
          errs ++
          if ismulticommodity
          then
          [ "Consider adjusting this entry's amounts, adding missing postings,"
          , "or recording conversion price(s) with @, @@ or equity postings." 
          ]
          else
          [ "Consider adjusting this entry's amounts, or adding missing postings."
          ]

transactionCommodities :: Transaction -> S.Set CommoditySymbol
transactionCommodities t = mconcat $ map (maCommodities . pamount) $ tpostings t

-- | Generate a transaction balancing error message, given the transaction
-- and one or more suberror messages.
transactionBalanceError :: Transaction -> [String] -> String
transactionBalanceError t errs = printf "%s:\n%s\n\nThis %stransaction is unbalanced.\n%s"
  (sourcePosPairPretty $ tsourcepos t)
  (textChomp ex)
  (if ismulticommodity then "multi-commodity " else "" :: String)
  (chomp $ unlines errs)
  where
    ismulticommodity = (length $ transactionCommodities t) > 1
    (_f,_l,_mcols,ex) = makeTransactionErrorExcerpt t finderrcols
      where
        finderrcols _ = Nothing
        -- finderrcols t = Just (1, Just w)
        --   where
        --     w = maximumDef 1 $ map T.length $ T.lines $ showTransaction t

-- | Infer up to one missing amount for this transactions's real postings, and
-- likewise for its balanced virtual postings, if needed; or return an error
-- message if we can't. Returns the updated transaction and any inferred posting amounts,
-- with the corresponding accounts, in order).
--
-- We can infer a missing amount when there are multiple postings and exactly
-- one of them is amountless. If the amounts had price(s) the inferred amount
-- have the same price(s), and will be converted to the price commodity.
transactionInferBalancingAmount ::
     M.Map CommoditySymbol AmountStyle -- ^ commodity display styles
  -> Transaction
  -> Either String (Transaction, [(AccountName, MixedAmount)])
transactionInferBalancingAmount styles t@Transaction{tpostings=ps}
  | length amountlessrealps > 1
      = Left $ transactionBalanceError t
        ["There can't be more than one real posting with no amount."
        ,"(Remember to put two or more spaces between account and amount.)"]
  | length amountlessbvps > 1
      = Left $ transactionBalanceError t
        ["There can't be more than one balanced virtual posting with no amount."
        ,"(Remember to put two or more spaces between account and amount.)"]
  | otherwise
      = let psandinferredamts = map inferamount ps
            inferredacctsandamts = [(paccount p, amt) | (p, Just amt) <- psandinferredamts]
        in Right (t{tpostings=map fst psandinferredamts}, inferredacctsandamts)
  where
    (amountfulrealps, amountlessrealps) = partition hasAmount (realPostings t)
    realsum = sumPostings amountfulrealps
    (amountfulbvps, amountlessbvps) = partition hasAmount (balancedVirtualPostings t)
    bvsum = sumPostings amountfulbvps

    inferamount :: Posting -> (Posting, Maybe MixedAmount)
    inferamount p =
      let
        minferredamt = case ptype p of
          RegularPosting         | not (hasAmount p) -> Just realsum
          BalancedVirtualPosting | not (hasAmount p) -> Just bvsum
          VirtualPosting         | not (hasAmount p) -> Just 0
          _                                          -> Nothing
      in
        case minferredamt of
          Nothing -> (p, Nothing)
          Just a  -> (p{pamount=a', poriginal=Just $ originalPosting p}, Just a')
            where
              -- Inferred amounts are converted to cost.
              -- Also ensure the new amount has the standard style for its commodity
              -- (since the main amount styling pass happened before this balancing pass);
              a' = mixedAmountSetStyles styles . mixedAmountCost $ maNegate a

-- | Infer costs for this transaction's posting amounts, if needed to make
-- the postings balance, and if permitted. This is done once for the real
-- postings and again (separately) for the balanced virtual postings. When
-- it's not possible, the transaction is left unchanged.
--
-- The simplest example is a transaction with two postings, each in a
-- different commodity, with no costs specified. In this case we'll add a
-- cost to the first posting such that it can be converted to the commodity
-- of the second posting (with -B), and such that the postings balance.
--
-- In general, we can infer a cost (conversion rate) when the sum of posting amounts
-- contains exactly two different commodities and no explicit costs.  Also
-- all postings are expected to contain an explicit amount (no missing
-- amounts) in a single commodity. Otherwise no cost inferring is attempted.
--
-- The transaction itself could contain more than two commodities, and/or
-- costs, if they cancel out; what matters is that the sum of posting amounts
-- contains exactly two commodities and zero costs.
--
-- There can also be more than two postings in either of the commodities.
--
-- We want to avoid excessive display of digits when the calculated cost is
-- an irrational number, while hopefully also ensuring the displayed numbers
-- make sense if the user does a manual calculation. This is (mostly) achieved
-- in two ways:
--
-- - when there is only one posting in the "from" commodity, a total cost
--   (@@) is used, and all available decimal digits are shown
--
-- - otherwise, a suitable averaged unit cost (@) is applied to the relevant
--   postings, with display precision equal to the summed display precisions
--   of the two commodities being converted between, or 2, whichever is larger.
--
-- (We don't always calculate a good-looking display precision for unit costs
-- when the commodity display precisions are low, eg when a journal doesn't
-- use any decimal places. The minimum of 2 helps make the costs shown by the
-- print command a bit less surprising in this case. Could do better.)
--
transactionInferBalancingCosts :: Transaction -> Transaction
transactionInferBalancingCosts t@Transaction{tpostings=ps} = t{tpostings=ps'}
  where
    ps' = map (costInferrerFor t BalancedVirtualPosting . costInferrerFor t RegularPosting) ps

-- | Generate a posting update function which assigns a suitable cost to
-- balance the posting, if and as appropriate for the given transaction and
-- posting type (real or balanced virtual) (or if we cannot or should not infer
-- costs, leaves the posting unchanged).
costInferrerFor :: Transaction -> PostingType -> (Posting -> Posting)
costInferrerFor t pt = maybe id infercost inferFromAndTo
  where
    postings     = filter ((==pt).ptype) $ tpostings t
    pcommodities = map acommodity $ concatMap (amounts . pamount) postings
    sumamounts   = amounts $ sumPostings postings  -- amounts normalises to one amount per commodity & price

    -- We can infer prices if there are no prices given, exactly two commodities in the normalised
    -- sum of postings in this transaction, and these two have opposite signs. The amount we are
    -- converting from is the first commodity to appear in the ordered list of postings, and the
    -- commodity we are converting to is the other. If we cannot infer prices, return Nothing.
    inferFromAndTo = case sumamounts of
      [a,b] | noprices, oppositesigns -> asum $ map orderIfMatches pcommodities
        where
          noprices      = all (isNothing . aprice) sumamounts
          oppositesigns = signum (aquantity a) /= signum (aquantity b)
          orderIfMatches x | x == acommodity a = Just (a,b)
                           | x == acommodity b = Just (b,a)
                           | otherwise         = Nothing
      _ -> Nothing

    -- For each posting, if the posting type matches, there is only a single amount in the posting,
    -- and the commodity of the amount matches the amount we're converting from,
    -- then set its cost based on the ratio between fromamount and toamount.
    infercost (fromamount, toamount) p
        | [a] <- amounts (pamount p), ptype p == pt, acommodity a == acommodity fromamount
            = p{ pamount   = mixedAmount a{aprice=Just conversionprice}
                     , poriginal = Just $ originalPosting p }
        | otherwise = p
      where
        -- If only one Amount in the posting list matches fromamount we can use TotalPrice.
        -- Otherwise divide the conversion equally among the Amounts by using a unit price.
        conversionprice = case filter (== acommodity fromamount) pcommodities of
            [_] -> TotalPrice $ negate toamount
            _   -> UnitPrice  $ negate unitprice `withPrecision` unitprecision

        unitprice     = aquantity fromamount `divideAmount` toamount
        unitprecision = case (asprecision $ astyle fromamount, asprecision $ astyle toamount) of
            (Precision a, Precision b) -> Precision . max 2 $ saturatedAdd a b
            _                          -> NaturalPrecision
        saturatedAdd a b = if maxBound - a < b then maxBound else a + b


-- | Check any balance assertions in the journal and return an error message
-- if any of them fail (or if the transaction balancing they require fails).
journalCheckBalanceAssertions :: Journal -> Maybe String
journalCheckBalanceAssertions = either Just (const Nothing) . journalBalanceTransactions defbalancingopts

-- "Transaction balancing", including: inferring missing amounts,
-- applying balance assignments, checking transaction balancedness,
-- checking balance assertions, respecting posting dates. These things
-- are all interdependent.
-- WARN tricky algorithm and code ahead. 
--
-- Code overview as of 20190219, this could/should be simplified/documented more:
--  parseAndFinaliseJournal['] (Cli/Utils.hs), journalAddForecast (Common.hs), journalAddBudgetGoalTransactions (BudgetReport.hs), tests (BalanceReport.hs)
--   journalBalanceTransactions
--    runST
--     runExceptT
--      balanceTransaction (Transaction.hs)
--       balanceTransactionHelper
--      runReaderT
--       balanceTransactionAndCheckAssertionsB
--        addAmountAndCheckAssertionB
--        addOrAssignAmountAndCheckAssertionB
--        balanceTransactionHelper (Transaction.hs)
--  uiCheckBalanceAssertions d ui@UIState{aopts=UIOpts{cliopts_=copts}, ajournal=j} (ErrorScreen.hs)
--   journalCheckBalanceAssertions
--    journalBalanceTransactions
--  transactionWizard, postingsBalanced (Add.hs), tests (Transaction.hs)
--   balanceTransaction (Transaction.hs)  XXX hledger add won't allow balance assignments + missing amount ?

-- | Monad used for statefully balancing/amount-inferring/assertion-checking
-- a sequence of transactions.
-- Perhaps can be simplified, or would a different ordering of layers make sense ?
-- If you see a way, let us know.
type Balancing s = ReaderT (BalancingState s) (ExceptT String (ST s))

-- | The state used while balancing a sequence of transactions.
data BalancingState s = BalancingState {
   -- read only
   bsStyles       :: Maybe (M.Map CommoditySymbol AmountStyle)  -- ^ commodity display styles
  ,bsUnassignable :: S.Set AccountName                          -- ^ accounts where balance assignments may not be used (because of auto posting rules)
  ,bsAssrt        :: Bool                                       -- ^ whether to check balance assertions
   -- mutable
  ,bsBalances     :: H.HashTable s AccountName MixedAmount      -- ^ running account balances, initially empty
  ,bsTransactions :: STArray s Integer Transaction              -- ^ a mutable array of the transactions being balanced
    -- (for efficiency ? journalBalanceTransactions says: not strictly necessary but avoids a sort at the end I think)
  }

-- | Access the current balancing state, and possibly modify the mutable bits,
-- lifting through the Except and Reader layers into the Balancing monad.
withRunningBalance :: (BalancingState s -> ST s a) -> Balancing s a
withRunningBalance f = ask >>= lift . lift . f

-- | Get this account's current exclusive running balance.
getRunningBalanceB :: AccountName -> Balancing s MixedAmount
getRunningBalanceB acc = withRunningBalance $ \BalancingState{bsBalances} -> do
  fromMaybe nullmixedamt <$> H.lookup bsBalances acc

-- | Add this amount to this account's exclusive running balance.
-- Returns the new running balance.
addToRunningBalanceB :: AccountName -> MixedAmount -> Balancing s MixedAmount
addToRunningBalanceB acc amt = withRunningBalance $ \BalancingState{bsBalances} -> do
  old <- fromMaybe nullmixedamt <$> H.lookup bsBalances acc
  let new = maPlus old amt
  H.insert bsBalances acc new
  return new

-- | Set this account's exclusive running balance to this amount.
-- Returns the change in exclusive running balance.
setRunningBalanceB :: AccountName -> MixedAmount -> Balancing s MixedAmount
setRunningBalanceB acc amt = withRunningBalance $ \BalancingState{bsBalances} -> do
  old <- fromMaybe nullmixedamt <$> H.lookup bsBalances acc
  H.insert bsBalances acc amt
  return $ maMinus amt old

-- | Set this account's exclusive running balance to whatever amount
-- makes its *inclusive* running balance (the sum of exclusive running
-- balances of this account and any subaccounts) be the given amount.
-- Returns the change in exclusive running balance.
setInclusiveRunningBalanceB :: AccountName -> MixedAmount -> Balancing s MixedAmount
setInclusiveRunningBalanceB acc newibal = withRunningBalance $ \BalancingState{bsBalances} -> do
  oldebal  <- fromMaybe nullmixedamt <$> H.lookup bsBalances acc
  allebals <- H.toList bsBalances
  let subsibal =  -- sum of any subaccounts' running balances
        maSum . map snd $ filter ((acc `isAccountNamePrefixOf`).fst) allebals
  let newebal = maMinus newibal subsibal
  H.insert bsBalances acc newebal
  return $ maMinus newebal oldebal

-- | Update (overwrite) this transaction in the balancing state.
updateTransactionB :: Transaction -> Balancing s ()
updateTransactionB t = withRunningBalance $ \BalancingState{bsTransactions}  ->
  void $ writeArray bsTransactions (tindex t) t

-- | Infer any missing amounts and/or conversion costs
-- (as needed to balance transactions and satisfy balance assignments);
-- and check that all transactions are balanced;
-- and (optional) check that all balance assertions pass.
-- Or, return an error message (just the first error encountered).
--
-- Assumes journalInferCommodityStyles has been called, since those
-- affect transaction balancing.
--
-- This does multiple things at once because amount inferring, balance
-- assignments, balance assertions and posting dates are interdependent.
journalBalanceTransactions :: BalancingOpts -> Journal -> Either String Journal
journalBalanceTransactions bopts' j' =
  let
    -- ensure transactions are numbered, so we can store them by number
    j@Journal{jtxns=ts} = journalNumberTransactions j'
    -- display precisions used in balanced checking
    styles = Just $ journalCommodityStyles j
    bopts = bopts'{commodity_styles_=styles}
    -- balance assignments are not allowed on accounts affected by auto postings
    autopostingaccts = S.fromList . map (paccount . tmprPosting) . concatMap tmpostingrules $ jtxnmodifiers j
  in
    -- Store the transactions in a mutable array, which we'll update as we balance them.
    -- Not strictly necessary but avoids a sort at the end I think.
    runST $ do
      balancedtxns <- newListArray (1, toInteger $ length ts) ts

      -- Process all transactions, or short-circuit with an error.
      runExceptT $ do

        -- Two passes are required:
        -- 1. Step through the transactions, balancing the ones which don't have balance assignments,
        -- postponing those which do until later. The balanced ones are split into their postings,
        -- keeping these and the not-yet-balanced transactions in the same relative order.
        psandts :: [Either Posting Transaction] <- fmap concat $ forM ts $ \case
          t | null $ assignmentPostings t -> case balanceTransaction bopts t of
              Left  e  -> throwError e
              Right t' -> do
                lift $ writeArray balancedtxns (tindex t') t'
                return $ map Left $ tpostings t'
          t -> return [Right t]

        -- 2. Step through these items in date order (and preserved same-day order),
        -- keeping running balances for all accounts.
        runningbals <- lift $ H.newSized (length $ journalAccountNamesUsed j)
        flip runReaderT (BalancingState styles autopostingaccts (not $ ignore_assertions_ bopts) runningbals balancedtxns) $ do
          -- On encountering any not-yet-balanced transaction with a balance assignment,
          -- enact the balance assignment then finish balancing the transaction.
          -- And, check any balance assertions encountered along the way.
          void $ mapM' balanceTransactionAndCheckAssertionsB $ sortOn (either postingDate tdate) psandts

        -- Return the now fully-balanced and checked transactions.
        ts' <- lift $ getElems balancedtxns
        return j{jtxns=ts'}

-- | This function is called statefully on each of a date-ordered sequence of
-- 1. fully explicit postings from already-balanced transactions and
-- 2. not-yet-balanced transactions containing balance assignments.
-- It executes balance assignments and finishes balancing the transactions,
-- and checks balance assertions on each posting as it goes.
-- An error will be thrown if a transaction can't be balanced
-- or if an illegal balance assignment is found (cf checkIllegalBalanceAssignment).
-- Transaction prices are removed, which helps eg balance-assertions.test: 15. Mix different commodities and assignments.
-- This stores the balanced transactions in case 2 but not in case 1.
balanceTransactionAndCheckAssertionsB :: Either Posting Transaction -> Balancing s ()
balanceTransactionAndCheckAssertionsB (Left p@Posting{}) =
  -- update the account's running balance and check the balance assertion if any
  void . addAmountAndCheckAssertionB $ postingStripPrices p
balanceTransactionAndCheckAssertionsB (Right t@Transaction{tpostings=ps}) = do
  -- make sure we can handle the balance assignments
  mapM_ checkIllegalBalanceAssignmentB ps
  -- for each posting, in date order (though without disturbing their display order),
  -- 1. infer its amount from the balance assignment if applicable,
  -- 2. update the account's running balance, and
  -- 3. check the balance assertion if any.
  ps' <- ps
    & zip [1..]                 -- attach original positions
    & sortOn (postingDate.snd)  -- sort by date
    & mapM (addOrAssignAmountAndCheckAssertionB)  -- infer amount, check assertion on each one
    <&> sortOn fst              -- restore original order
    <&> map snd                 -- discard positions

  -- infer any remaining missing amounts, and make sure the transaction is now fully balanced
  styles <- R.reader bsStyles
  case balanceTransactionHelper defbalancingopts{commodity_styles_=styles} t{tpostings=ps'} of
    Left err -> throwError err
    Right (t', inferredacctsandamts) -> do
      -- for each amount just inferred, update the running balance
      mapM_ (uncurry addToRunningBalanceB) inferredacctsandamts
      -- and save the balanced transaction.
      updateTransactionB t'

type NumberedPosting = (Integer, Posting)

-- | If this posting has an explicit amount, add it to the account's running balance.
-- If it has a missing amount and a balance assignment, infer the amount from, and
-- reset the running balance to, the assigned balance.
-- If it has a missing amount and no balance assignment, leave it for later.
-- Then test the balance assertion if any.
addOrAssignAmountAndCheckAssertionB :: NumberedPosting -> Balancing s NumberedPosting
addOrAssignAmountAndCheckAssertionB (i,p@Posting{paccount=acc, pamount=amt, pbalanceassertion=mba})
  -- an explicit posting amount
  | hasAmount p = do
      newbal <- addToRunningBalanceB acc amt
      whenM (R.reader bsAssrt) $ checkBalanceAssertionB p newbal
      return (i,p)

  -- no explicit posting amount, but there is a balance assignment
  | Just BalanceAssertion{baamount,batotal,bainclusive} <- mba = do
      newbal <- if batotal
                   -- a total balance assignment (==, all commodities)
                   then return $ mixedAmount baamount
                   -- a partial balance assignment (=, one commodity)
                   else do
                     oldbalothercommodities <- filterMixedAmount ((acommodity baamount /=) . acommodity) <$> getRunningBalanceB acc
                     return $ maAddAmount oldbalothercommodities baamount
      diff <- (if bainclusive then setInclusiveRunningBalanceB else setRunningBalanceB) acc newbal
      let p' = p{pamount=filterMixedAmount (not . amountIsZero) diff, poriginal=Just $ originalPosting p}
      whenM (R.reader bsAssrt) $ checkBalanceAssertionB p' newbal
      return (i,p')

  -- no explicit posting amount, no balance assignment
  | otherwise = return (i,p)

-- | Add the posting's amount to its account's running balance, and
-- optionally check the posting's balance assertion if any.
-- The posting is expected to have an explicit amount (otherwise this does nothing).
-- Adding and checking balance assertions are tightly paired because we
-- need to see the balance as it stands after each individual posting.
addAmountAndCheckAssertionB :: Posting -> Balancing s Posting
addAmountAndCheckAssertionB p | hasAmount p = do
  newbal <- addToRunningBalanceB (paccount p) $ pamount p
  whenM (R.reader bsAssrt) $ checkBalanceAssertionB p newbal
  return p
addAmountAndCheckAssertionB p = return p

-- | Check a posting's balance assertion against the given actual balance, and
-- return an error if the assertion is not satisfied.
-- If the assertion is partial, unasserted commodities in the actual balance
-- are ignored; if it is total, they will cause the assertion to fail.
checkBalanceAssertionB :: Posting -> MixedAmount -> Balancing s ()
checkBalanceAssertionB p@Posting{pbalanceassertion=Just (BalanceAssertion{baamount,batotal})} actualbal =
    forM_ (baamount : otheramts) $ \amt -> checkBalanceAssertionOneCommodityB p amt actualbal
  where
    assertedcomm = acommodity baamount
    otheramts | batotal   = map (\a -> a{aquantity=0}) . amountsRaw
                          $ filterMixedAmount ((/=assertedcomm).acommodity) actualbal
              | otherwise = []
checkBalanceAssertionB _ _ = return ()

-- | Does this (single commodity) expected balance match the amount of that
-- commodity in the given (multicommodity) actual balance ? If not, returns a
-- balance assertion failure message based on the provided posting.  To match,
-- the amounts must be exactly equal (display precision is ignored here).
-- If the assertion is inclusive, the expected amount is compared with the account's
-- subaccount-inclusive balance; otherwise, with the subaccount-exclusive balance.
checkBalanceAssertionOneCommodityB :: Posting -> Amount -> MixedAmount -> Balancing s ()
checkBalanceAssertionOneCommodityB p@Posting{paccount=assertedacct} assertedamt actualbal = do
  let isinclusive = maybe False bainclusive $ pbalanceassertion p
  let istotal     = maybe False batotal     $ pbalanceassertion p
  actualbal' <-
    if isinclusive
    then
      -- sum the running balances of this account and any of its subaccounts seen so far
      withRunningBalance $ \BalancingState{bsBalances} ->
        H.foldM
          (\ibal (acc, amt) -> return $
            if assertedacct==acc || assertedacct `isAccountNamePrefixOf` acc then maPlus ibal amt else ibal)
          nullmixedamt
          bsBalances
    else return actualbal
  let
    assertedcomm    = acommodity assertedamt
    actualbalincomm = headDef nullamt . amountsRaw . filterMixedAmountByCommodity assertedcomm $ actualbal'
    pass =
      aquantity
        -- traceWith (("asserted:"++).showAmountDebug)
        assertedamt ==
      aquantity
        -- traceWith (("actual:"++).showAmountDebug)
        actualbalincomm
    errmsg = chomp $ printf (unlines
      [ "%s:",
        "%s\n",
        "This balance assertion failed.",
        -- "date:       %s",
        "In account:    %s",
        "and commodity: %s",
        -- "display precision:  %d",
        "this balance was asserted:     %s", -- (at display precision: %s)",
        "but the calculated balance is: %s", -- (at display precision: %s)",
        "a difference of:               %s",
        "",
        "Consider viewing this account's calculated balances to troubleshoot. Eg:",
        "",
        "hledger reg '%s'%s -I  # -f FILE"
      ])
      (sourcePosPretty pos)
      (textChomp ex)
      -- (showDate $ postingDate p)
      (if isinclusive then printf "%-30s  (including subaccounts)" acct else acct)
      (if istotal     then printf "%-30s  (no other commodities allowed)" (T.unpack assertedcomm) else (T.unpack assertedcomm))
      -- (asprecision $ astyle actualbalincommodity)  -- should be the standard display precision I think
      (show $ aquantity assertedamt)
      -- (showAmount assertedamt)
      (show $ aquantity actualbalincomm)
      -- (showAmount actualbalincommodity)
      (show $ aquantity assertedamt - aquantity actualbalincomm)
      (acct ++ if isinclusive then "" else "$")
      (if istotal then "" else (" cur:" ++ quoteForCommandLine (T.unpack assertedcomm)))
      where
        acct = T.unpack $ paccount p
        ass = fromJust $ pbalanceassertion p  -- PARTIAL: fromJust won't fail, there is a balance assertion
        pos = baposition ass
        (_,_,_,ex) = makeBalanceAssertionErrorExcerpt p
  unless pass $ throwError errmsg

-- | Throw an error if this posting is trying to do an illegal balance assignment.
checkIllegalBalanceAssignmentB :: Posting -> Balancing s ()
checkIllegalBalanceAssignmentB p = do
  checkBalanceAssignmentPostingDateB p
  checkBalanceAssignmentUnassignableAccountB p

-- XXX these should show position. annotateErrorWithTransaction t ?

-- | Throw an error if this posting is trying to do a balance assignment and
-- has a custom posting date (which makes amount inference too hard/impossible).
checkBalanceAssignmentPostingDateB :: Posting -> Balancing s ()
checkBalanceAssignmentPostingDateB p =
  when (hasBalanceAssignment p && isJust (pdate p)) $
    throwError $ chomp $ unlines [
       "Balance assignments and custom posting dates may not be combined."
      ,""
      ,chomp1 $ T.unpack $ maybe (T.unlines $ showPostingLines p) showTransaction $ ptransaction p
      ,"Balance assignments may not be used on postings with a custom posting date"
      ,"(it makes balancing the journal impossible)."
      ,"Please write the posting amount explicitly (or remove the posting date)."
      ]

-- | Throw an error if this posting is trying to do a balance assignment and
-- the account does not allow balance assignments (eg because it is referenced
-- by an auto posting rule, which might generate additional postings to it).
checkBalanceAssignmentUnassignableAccountB :: Posting -> Balancing s ()
checkBalanceAssignmentUnassignableAccountB p = do
  unassignable <- R.asks bsUnassignable
  when (hasBalanceAssignment p && paccount p `S.member` unassignable) $
    throwError $ chomp $ unlines [
       "Balance assignments and auto postings may not be combined."
      ,""
      ,chomp1 $ T.unpack $ maybe (T.unlines $ showPostingLines p) (showTransaction) $ ptransaction p
      ,"Balance assignments may not be used on accounts affected by auto posting rules"
      ,"(it makes balancing the journal impossible)."
      ,"Please write the posting amount explicitly (or remove the auto posting rule(s))."
      ]

-- lenses

makeHledgerClassyLenses ''BalancingOpts

-- tests

tests_Balancing :: TestTree
tests_Balancing =
  testGroup "Balancing" [

      testCase "transactionInferBalancingAmount" $ do
         (fst <$> transactionInferBalancingAmount M.empty nulltransaction) @?= Right nulltransaction
         (fst <$> transactionInferBalancingAmount M.empty nulltransaction{tpostings = ["a" `post` usd (-5), "b" `post` missingamt]}) @?=
           Right nulltransaction{tpostings = ["a" `post` usd (-5), "b" `post` usd 5]}
         (fst <$> transactionInferBalancingAmount M.empty nulltransaction{tpostings = ["a" `post` usd (-5), "b" `post` (eur 3 @@ usd 4), "c" `post` missingamt]}) @?=
           Right nulltransaction{tpostings = ["a" `post` usd (-5), "b" `post` (eur 3 @@ usd 4), "c" `post` usd 1]}

    , testGroup "balanceTransaction" [
         testCase "detect unbalanced entry, sign error" $
          assertLeft
            (balanceTransaction defbalancingopts
               (Transaction
                  0
                  ""
                  nullsourcepos
                  (fromGregorian 2007 01 28)
                  Nothing
                  Unmarked
                  ""
                  "test"
                  ""
                  []
                  [posting {paccount = "a", pamount = mixedAmount (usd 1)}, posting {paccount = "b", pamount = mixedAmount (usd 1)}]))
        ,testCase "detect unbalanced entry, multiple missing amounts" $
          assertLeft $
             balanceTransaction defbalancingopts
               (Transaction
                  0
                  ""
                  nullsourcepos
                  (fromGregorian 2007 01 28)
                  Nothing
                  Unmarked
                  ""
                  "test"
                  ""
                  []
                  [ posting {paccount = "a", pamount = missingmixedamt}
                  , posting {paccount = "b", pamount = missingmixedamt}
                  ])
        ,testCase "one missing amount is inferred" $
          (pamount . last . tpostings <$>
           balanceTransaction defbalancingopts
             (Transaction
                0
                ""
                nullsourcepos
                (fromGregorian 2007 01 28)
                Nothing
                Unmarked
                ""
                ""
                ""
                []
                [posting {paccount = "a", pamount = mixedAmount (usd 1)}, posting {paccount = "b", pamount = missingmixedamt}])) @?=
          Right (mixedAmount $ usd (-1))
        ,testCase "conversion price is inferred" $
          (pamount . head . tpostings <$>
           balanceTransaction defbalancingopts
             (Transaction
                0
                ""
                nullsourcepos
                (fromGregorian 2007 01 28)
                Nothing
                Unmarked
                ""
                ""
                ""
                []
                [ posting {paccount = "a", pamount = mixedAmount (usd 1.35)}
                , posting {paccount = "b", pamount = mixedAmount (eur (-1))}
                ])) @?=
          Right (mixedAmount $ usd 1.35 @@ eur 1)
        ,testCase "balanceTransaction balances based on cost if there are unit prices" $
          assertRight $
          balanceTransaction defbalancingopts
            (Transaction
               0
               ""
               nullsourcepos
               (fromGregorian 2011 01 01)
               Nothing
               Unmarked
               ""
               ""
               ""
               []
               [ posting {paccount = "a", pamount = mixedAmount $ usd 1 `at` eur 2}
               , posting {paccount = "a", pamount = mixedAmount $ usd (-2) `at` eur 1}
               ])
        ,testCase "balanceTransaction balances based on cost if there are total prices" $
          assertRight $
          balanceTransaction defbalancingopts
            (Transaction
               0
               ""
               nullsourcepos
               (fromGregorian 2011 01 01)
               Nothing
               Unmarked
               ""
               ""
               ""
               []
               [ posting {paccount = "a", pamount = mixedAmount $ usd 1 @@ eur 1}
               , posting {paccount = "a", pamount = mixedAmount $ usd (-2) @@ eur (-1)}
               ])
        ]
    , testGroup "isTransactionBalanced" [
         testCase "detect balanced" $
          assertBool "" $
          isTransactionBalanced defbalancingopts $
          Transaction
            0
            ""
            nullsourcepos
            (fromGregorian 2009 01 01)
            Nothing
            Unmarked
            ""
            "a"
            ""
            []
            [ posting {paccount = "b", pamount = mixedAmount (usd 1.00)}
            , posting {paccount = "c", pamount = mixedAmount (usd (-1.00))}
            ]
        ,testCase "detect unbalanced" $
          assertBool "" $
          not $
          isTransactionBalanced defbalancingopts $
          Transaction
            0
            ""
            nullsourcepos
            (fromGregorian 2009 01 01)
            Nothing
            Unmarked
            ""
            "a"
            ""
            []
            [ posting {paccount = "b", pamount = mixedAmount (usd 1.00)}
            , posting {paccount = "c", pamount = mixedAmount (usd (-1.01))}
            ]
        ,testCase "detect unbalanced, one posting" $
          assertBool "" $
          not $
          isTransactionBalanced defbalancingopts $
          Transaction
            0
            ""
            nullsourcepos
            (fromGregorian 2009 01 01)
            Nothing
            Unmarked
            ""
            "a"
            ""
            []
            [posting {paccount = "b", pamount = mixedAmount (usd 1.00)}]
        ,testCase "one zero posting is considered balanced for now" $
          assertBool "" $
          isTransactionBalanced defbalancingopts $
          Transaction
            0
            ""
            nullsourcepos
            (fromGregorian 2009 01 01)
            Nothing
            Unmarked
            ""
            "a"
            ""
            []
            [posting {paccount = "b", pamount = mixedAmount (usd 0)}]
        ,testCase "virtual postings don't need to balance" $
          assertBool "" $
          isTransactionBalanced defbalancingopts $
          Transaction
            0
            ""
            nullsourcepos
            (fromGregorian 2009 01 01)
            Nothing
            Unmarked
            ""
            "a"
            ""
            []
            [ posting {paccount = "b", pamount = mixedAmount (usd 1.00)}
            , posting {paccount = "c", pamount = mixedAmount (usd (-1.00))}
            , posting {paccount = "d", pamount = mixedAmount (usd 100), ptype = VirtualPosting}
            ]
        ,testCase "balanced virtual postings need to balance among themselves" $
          assertBool "" $
          not $
          isTransactionBalanced defbalancingopts $
          Transaction
            0
            ""
            nullsourcepos
            (fromGregorian 2009 01 01)
            Nothing
            Unmarked
            ""
            "a"
            ""
            []
            [ posting {paccount = "b", pamount = mixedAmount (usd 1.00)}
            , posting {paccount = "c", pamount = mixedAmount (usd (-1.00))}
            , posting {paccount = "d", pamount = mixedAmount (usd 100), ptype = BalancedVirtualPosting}
            ]
        ,testCase "balanced virtual postings need to balance among themselves (2)" $
          assertBool "" $
          isTransactionBalanced defbalancingopts $
          Transaction
            0
            ""
            nullsourcepos
            (fromGregorian 2009 01 01)
            Nothing
            Unmarked
            ""
            "a"
            ""
            []
            [ posting {paccount = "b", pamount = mixedAmount (usd 1.00)}
            , posting {paccount = "c", pamount = mixedAmount (usd (-1.00))}
            , posting {paccount = "d", pamount = mixedAmount (usd 100), ptype = BalancedVirtualPosting}
            , posting {paccount = "3", pamount = mixedAmount (usd (-100)), ptype = BalancedVirtualPosting}
            ]
        ]

  ,testGroup "journalBalanceTransactions" [

     testCase "missing-amounts" $ do
      let ej = journalBalanceTransactions defbalancingopts $ samplejournalMaybeExplicit False
      assertRight ej
      journalPostings <$> ej @?= Right (journalPostings samplejournal)

    ,testCase "balance-assignment" $ do
      let ej = journalBalanceTransactions defbalancingopts $
            --2019/01/01
            --  (a)            = 1
            nulljournal{ jtxns = [
              transaction (fromGregorian 2019 01 01) [ vpost' "a" missingamt (balassert (num 1)) ]
            ]}
      assertRight ej
      case ej of Right j -> (jtxns j & head & tpostings & head & pamount & amountsRaw) @?= [num 1]
                 Left _  -> error' "balance-assignment test: shouldn't happen"

    ,testCase "same-day-1" $ do
      assertRight $ journalBalanceTransactions defbalancingopts $
            --2019/01/01
            --  (a)            = 1
            --2019/01/01
            --  (a)          1 = 2
            nulljournal{ jtxns = [
               transaction (fromGregorian 2019 01 01) [ vpost' "a" missingamt (balassert (num 1)) ]
              ,transaction (fromGregorian 2019 01 01) [ vpost' "a" (num 1)    (balassert (num 2)) ]
            ]}

    ,testCase "same-day-2" $ do
      assertRight $ journalBalanceTransactions defbalancingopts $
            --2019/01/01
            --    (a)                  2 = 2
            --2019/01/01
            --    b                    1
            --    a
            --2019/01/01
            --    a                    0 = 1
            nulljournal{ jtxns = [
               transaction (fromGregorian 2019 01 01) [ vpost' "a" (num 2)    (balassert (num 2)) ]
              ,transaction (fromGregorian 2019 01 01) [
                 post' "b" (num 1)     Nothing
                ,post' "a"  missingamt Nothing
              ]
              ,transaction (fromGregorian 2019 01 01) [ post' "a" (num 0)     (balassert (num 1)) ]
            ]}

    ,testCase "out-of-order" $ do
      assertRight $ journalBalanceTransactions defbalancingopts $
            --2019/1/2
            --  (a)    1 = 2
            --2019/1/1
            --  (a)    1 = 1
            nulljournal{ jtxns = [
               transaction (fromGregorian 2019 01 02) [ vpost' "a" (num 1)    (balassert (num 2)) ]
              ,transaction (fromGregorian 2019 01 01) [ vpost' "a" (num 1)    (balassert (num 1)) ]
            ]}

    ]

    ,testGroup "commodityStylesFromAmounts" $ [

      -- Journal similar to the one on #1091:
      -- 2019/09/24
      --     (a)            1,000.00
      -- 
      -- 2019/09/26
      --     (a)             1000,000
      --
      testCase "1091a" $ do
        commodityStylesFromAmounts [
           nullamt{aquantity=1000, astyle=AmountStyle L False (Precision 3) (Just ',') Nothing}
          ,nullamt{aquantity=1000, astyle=AmountStyle L False (Precision 2) (Just '.') (Just (DigitGroups ',' [3]))}
          ]
         @?=
          -- The commodity style should have period as decimal mark
          -- and comma as digit group mark.
          Right (M.fromList [
            ("", AmountStyle L False (Precision 3) (Just '.') (Just (DigitGroups ',' [3])))
          ])
        -- same journal, entries in reverse order
      ,testCase "1091b" $ do
        commodityStylesFromAmounts [
           nullamt{aquantity=1000, astyle=AmountStyle L False (Precision 2) (Just '.') (Just (DigitGroups ',' [3]))}
          ,nullamt{aquantity=1000, astyle=AmountStyle L False (Precision 3) (Just ',') Nothing}
          ]
         @?=
          -- The commodity style should have period as decimal mark
          -- and comma as digit group mark.
          Right (M.fromList [
            ("", AmountStyle L False (Precision 3) (Just '.') (Just (DigitGroups ',' [3])))
          ])

     ]

  ]
