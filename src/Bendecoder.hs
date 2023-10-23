
module Bendecoder (decodeBencodedValue) where
import Data.Char

decodeBencodedValue :: String -> String
decodeBencodedValue ('i':ss) = decodeNumber (ss)
-- decodeBencodedValue ('l':ss) = decodeList(ss)
decodeBencodedValue ss = decodeString (read (getNumberPrefix ss), removeNumberPrefix ss)

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
