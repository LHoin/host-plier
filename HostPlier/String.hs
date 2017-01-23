module HostPlier.String (
  isIp,
  trim,
  startsWith
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

