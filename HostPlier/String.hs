module HostPlier.String (
  isIp,
  trim,
  startsWith,
  replace
) where

import Text.Regex.PCRE
import Data.Char (isSpace)

isIp :: String -> Bool
isIp x = x =~ "^(25[0-5]|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[1-9])\\.(25[0-5]|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[1-9]|0)\\.(25[0-5]|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[1-9]|0)\\.(25[0-5]|2[0-4][0-9]|[0-1]{1}[0-9]{2}|[1-9]{1}[0-9]{1}|[0-9])$"

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

startsWith :: String -> String -> Bool
startsWith _ [] = True
startsWith [] _ = False
startsWith (hl:tl) (hs:ts) 
  | hl == hs = startsWith tl ts
  | otherwise = False

replace :: String -> String -> String -> String
replace _ _ [] = ""
replace _ [] _ = ""
replace newWord wordv strv = _replace ("", wordv, strv) wordv strv
  where
    _replace (result, word, str) (w:ws) (s:ss)
      | (length word) > (length str) = result ++ str
      | wordLeft == 0 && strLeft == 0 && w /= s = result ++ str 
      | wordLeft == 0 && strLeft == 0 && w == s = result ++ newWord
      | wordLeft == 0 && w == s = 
          let 
             nextStr = drop (length word) str
          in _replace (result ++ newWord, word, nextStr) word nextStr
      | wordLeft > 0 && w == s = _replace (result, word, str) ws ss
      | w /= s = 
          let 
             nextStr = tail str 
          in _replace (result ++ (take 1 str), word, nextStr) word nextStr
      where
        wordLeft = length ws
        strLeft = length ss
