module Bendecoder (decodeBencodedValue, replace,decodeString,decodeNumber,getNumberPrefix,removeNumberPrefix,removeUntilEnd) where
import Data.Char
import Data.List


decodeBencodedValue :: String -> String
decodeBencodedValue [] = ""
decodeBencodedValue ('i':ss) = decodeNumber ss ++ "," ++ decodeBencodedValue (removeUntilEnd ss) 
decodeBencodedValue ('e':ss) = "]" ++ decodeBencodedValue ss
decodeBencodedValue ('l':ss) = "[" ++ decodeBencodedValue ss
decodeBencodedValue (s:ss)
  | isNumber s = decodeString(s:ss)
  | otherwise = decodeBencodedValue ss


decodeNumber :: String -> String
decodeNumber ('e':ss) = ""
decodeNumber (s:ss) = s : decodeNumber ss

decodeString :: String -> String
decodeString (s:ss) 
  | isNumber s = readStringWithLen (read (getNumberPrefix( s:ss)), removeNumberPrefix (s:ss))
  | otherwise = error "string without len number.\nUsage: <number:some_value>"

readStringWithLen :: (Integer,String) -> String
readStringWithLen (0, []) = ""
readStringWithLen (_, []) = ""
readStringWithLen (0, _) = ""
readStringWithLen (len,':':ss) = readStringWithLen (len,ss)
readStringWithLen (len,s:ss) = s : readStringWithLen (len-1,ss)


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