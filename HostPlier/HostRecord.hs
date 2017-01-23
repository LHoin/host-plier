module HostPlier.HostRecord (
  HostRecord (..),
  valueRecordDefault
) where

import HostPlier.List

data HostRecord = ValueRecord {
  ip :: String,
  hostName :: [String],
  group :: String,
  open :: Bool,
  originText:: String
} | TextRecord {
  originText:: String
}

valueRecordDefault = ValueRecord {
  ip = "",
  hostName = [],
  group = "",
  open = False,
  originText = ""
}

instance Show HostRecord where
  show (TextRecord {originText=text}) = text ++ "\n"
  show r@ValueRecord {} = comment ++ (ip r) ++ " " ++ (join " " $ hostName r) ++ "\n"
    where
      comment = if open r then "" else "#"
