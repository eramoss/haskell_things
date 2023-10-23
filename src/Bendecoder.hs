
module Bendecoder (decodeBencodedValue, replace) where
import Data.Char
import Data.List



decodeBencodedValue :: String -> String
decodeBencodedValue [] = ""
decodeBencodedValue ('i':ss) = decodeNumber ss ++ "," ++ decodeBencodedValue (removeUntilEnd ss) 
decodeBencodedValue ('e':ss) = "]" ++ decodeBencodedValue ss
decodeBencodedValue ('l':ss) = "[" ++ decodeBencodedValue ss
decodeBencodedValue (s:ss)
  | isNumber s = decodedString ++ ","  ++ decodeBencodedValue (remove decodedString ss)
  | otherwise = decodeBencodedValue ss
  where
    decodedString = decodeString (read (getNumberPrefix( s:ss)), removeNumberPrefix (s:ss))

decodeNumber :: String -> String
decodeNumber ('e':ss) = ""
decodeNumber (s:ss) = s : decodeNumber ss

decodeString :: (Integer,String) -> String
decodeString (_, []) = "" -- end the string but not counter
decodeString (0, _) = "" -- end the counter
decodeString (len,':':ss) = decodeString (len,ss)
decodeString (len,s:ss) = s : decodeString (len-1,ss)

getNumberPrefix:: String -> String
getNumberPrefix (s:ss)
  | isNumber s = s : getNumberPrefix ss
  | otherwise = ""

removeNumberPrefix :: String -> String
removeNumberPrefix (':':ss) = ss
removeNumberPrefix (_:ss) = removeNumberPrefix ss

removeUntilEnd :: String -> String
removeUntilEnd [] = ""
removeUntilEnd ('e':ss) = ss
removeUntilEnd (_:ss) = removeUntilEnd ss

replace :: String -> String -> String -> String
replace _ _ [] = []
replace find repl input@(x:xs)
    | isPrefixOf find input = repl ++ replace find repl (drop (length find) input)
    | otherwise = x : replace find repl xs


remove :: String -> String -> String
remove w "" = ""
remove w s@(c:cs) 
  | w `isPrefixOf` s = remove w (drop (length w) s)
  | otherwise = c : remove w cs