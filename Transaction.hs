
module Transaction
where

import Debug.Trace
import Text.Printf
import Text.Regex
import Data.List

import Utils
import BasicTypes
import Account


data Transaction = Transaction {
                                taccount :: AccountName,
                                tamount :: Amount
                               } deriving (Eq,Ord)

instance Show Transaction where show = showTransaction

showTransaction t = (showAccountName $ taccount t) ++ "  " ++ (showAmount $ tamount t) 
showAmount amt = printf "%11s" (show amt)
showAccountName s = printf "%-22s" (elideRight 22 s)

elideRight width s =
    case length s > width of
      True -> take (width - 2) s ++ ".."
      False -> s

-- elideAccountRight width abbrevlen a = 
--     case length a > width of
--       False -> a
--       True -> abbreviateAccountComponent abbrevlen a 
        
-- abbreviateAccountComponent abbrevlen a =
--     let components = splitAtElement ':' a in
--     case 
    
autofillTransactions :: [Transaction] -> [Transaction]
autofillTransactions ts =
    let (ns, as) = partition isNormal ts
            where isNormal t = (currency $ tamount t) /= "AUTO" in
    case (length as) of
      0 -> ns
      1 -> ns ++ [balanceTransaction $ head as]
          where balanceTransaction t = t{tamount = -(sumTransactions ns)}
      otherwise -> error "too many blank transactions in this entry"

sumTransactions :: [Transaction] -> Amount
sumTransactions ts = sum [tamount t | t <- ts]

