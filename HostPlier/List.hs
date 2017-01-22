module HostPlier.List (
  join,
  split
) where

join spliter xs
  | xs == [] = ""
  | otherwise = tail $ foldl (_join spliter) "" xs
  where
    _join spliter result str = result ++ spliter ++ str

split :: [Char] -> String -> [String]
split spliter "" = []
split spliter str = foldl (_split spliter) [""] str
  where 
    _split :: [Char] -> [String] -> Char -> [String]
    _split spliter result char
      | (elem char spliter) = result ++ [""]
      | otherwise = (init result) ++ [(last result) ++ [char]]
