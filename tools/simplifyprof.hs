#!/usr/bin/env runhaskell
-- filters uninteresting fields from profile data lines
-- Simon Michael 2007
import Data.List
main = interact $ unlines . map print . filter (/=[]) . 
       (["cost-centre - - entries %time %mem %t-inh %m-inh"]++) . tail . 
       dropWhile (notElem "entries" . words) . lines
    where
      print line = tabcat [paddedfirst, field 3, field 4, field 5, field 6, field 7]
          where
            tabcat = concat . intersperse "\t"
            first = takeWhile (==' ') line ++ (takeWhile (/=' ') $ dropWhile (==' ') line)
            paddedfirst = first ++ (take (60 - (length first)) $ repeat ' ')
            field n = words line !! n
