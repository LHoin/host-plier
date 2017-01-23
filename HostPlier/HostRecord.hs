module HostPlier.HostRecord (
  HostRecord (..),
  valueRecordDefault,
  toRecord
) where

import Text.Regex.PCRE
import HostPlier.List
import HostPlier.String

data HostRecord = ValueRecord {
  ip :: String,
  hostName :: [String],
  group :: String,
  open :: Bool,
  originText:: String,
  comment :: String
} | TextRecord {
  originText:: String
}

valueRecordDefault = ValueRecord {
  ip = "",
  hostName = [],
  group = "",
  open = False,
  originText = "",
  comment = ""
}

instance Show HostRecord where
  show (TextRecord {originText=text}) = text ++ "\n"
  show ValueRecord {open=openValue, ip=ipValue, hostName=hostNameValue, comment=commentValue} = 
    tag ++ ipValue ++ " " ++ (join " " hostNameValue) ++ " " ++ commentValue ++ "\n"
    where
      tag = if openValue then "" else "#"


formatHostNames xs = foldl _format [] xs
  where
    _format result x
      | (trim x) == "" = result
      | otherwise = result ++ [x]

getComment :: String -> String
getComment x = x =~ "#.*"

removeComment :: String -> String
removeComment x = x =~ "(.*?(?=#))|(.*)" :: String

toRecord :: String -> String -> HostRecord
toRecord group line
  | isValid = valueRecordDefault {
      originText = trimedLine,
      open = open,
      ip = ip,
      hostName = formatHostNames $ tail elements,
      group = group,
      comment = comment 
    } 
  | otherwise = TextRecord {
      originText = trimedLine
    } 
  where 
    trimedLine = trim line
    open = (head trimedLine) /= '#'
    info
      | not open = tail trimedLine
      | otherwise = trimedLine
    elements = split " \t" $ removeComment info 
    comment = getComment info 
    len = length elements
    ip = elements !! 0 
    isValid
      | line /= "" && len > 1 = isIp ip 
      | otherwise = False
