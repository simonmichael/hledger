{-|

A 'Commodity' is a symbol representing a currency or some other kind of
thing we are tracking, and some display preferences that tell how to
display 'Amount's of the commodity - is the symbol on the left or right,
are thousands separated by comma, significant decimal places and so on.

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Hledger.Data.Commodity
where
import Data.Char (isDigit)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import qualified Data.Text as T
import Test.HUnit


-- should we specifiy `isDecimalPointChar c` rather than explicitly
-- listing '.' and ','?
isNonsimpleCommodityChar :: Char -> Bool
isNonsimpleCommodityChar c = isDigit c || c `textElem` otherChars
 where
   otherChars = "-+.,@*;\n \"{}=" :: T.Text
   textElem = T.any . (==)

quoteCommoditySymbolIfNeeded s | T.any (isNonsimpleCommodityChar) s = "\"" <> s <> "\""
                               | otherwise = s

tests_Hledger_Data_Commodity = TestList [
 ]
