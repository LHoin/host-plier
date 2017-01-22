import System.Environment
import System.IO
import HostPlier.String
import HostPlier.List
import HostPlier.HostRecord

formatHostNames xs = foldl _format [] xs
  where
    _format result x
      | (trim x) == "" = result
      | otherwise = result ++ [x]


toRecord :: String -> String -> HostRecord
toRecord group line
  | isValid = ValueRecord {
      originText = trimedLine,
      open = open,
      ip = ip,
      hostName = formatHostNames $ tail elements,
      group = group
    } 
  | otherwise = TextRecord {
      originText = trimedLine
    } 
  where 
    trimedLine = trim line
    open = (head trimedLine) /= '#'
    elements 
      | not open =  split " \t" (tail trimedLine)
      | otherwise = split " \t" trimedLine
    len = length elements
    ip = elements !! 0 
    isValid
      | line /= "" && len > 1 = isIp ip 
      | otherwise = False
   

toLinesWithGroup :: [(String, String)] -> String -> [(String, String)]
toLinesWithGroup result line 
  | (trim line) == "" = result
  | result == [] = result ++ [(line, "")]
  | (isGroup || isGroupEnd) && lastLine == "" = (init result) ++ [("", groupName)]
  | isGroup || isGroupEnd = result ++ [("", groupName)]
  | lastLine == "" = (init result) ++ [(line, lastGroupName)]
  | otherwise = result ++ [(line, lastGroupName)]
  where
    isGroup = startsWith line "#====" 
    groupName
      | isGroup = trim $ drop 5 line
      | otherwise = ""
    isGroupEnd = isGroup && (groupName == "")
    lastElement = last result 
    lastLine = fst lastElement
    lastGroupName = snd lastElement

toLinesGroup = foldl toLinesWithGroup []

toRecords :: [(String, String)] -> [HostRecord]
toRecords lines = foldl _toRecords [] lines
  where 
    _toRecords result line = result ++ (splitRecord record)
      where
        recordLine = fst line
        groupValue = snd line
        record = toRecord groupValue recordLine
        splitRecord r@TextRecord {} = [r]
        splitRecord r@ValueRecord {}
          | (length hostNameValue) < 2 = [r]
          | otherwise = map (\h -> r {hostName = [h]}) hostNameValue 
          where
            hostNameValue = hostName r 

toHostFile recordsIdx
  | (length recordsIdx) < 1  = ""
  | otherwise = foldl _step "" recordsIdx
  where
    _step result (_, TextRecord {originText=text}) = result ++ text ++ "\n"
    _step result (index, r@ValueRecord {}) 
      | groupValue == "" && lastGroupValue /= "" = result ++ "#====\n\n" ++ (show r)
      | groupValue /= "" && lastGroupValue == "" = result ++ "\n#==== " ++ groupValue ++ "\n" ++(show r)
      | groupValue == lastGroupValue = result ++ (show r)
      | groupValue /= lastGroupValue = result ++ "#====\n\n#==== " ++ groupValue ++ "\n" ++ (show r)
      where 
        _group r@TextRecord {} = ""
        _group r@ValueRecord {} = group r 
        groupValue = _group r
        lastGroupValue  
          | index == 0 = ""
          | otherwise = _group $ snd (recordsIdx !! (index - 1))

makeString str result r@(TextRecord {}) = result 
makeString str result r@(ValueRecord {}) = result ++ " " ++ (str r)

filterValueRecord f TextRecord {} = False 
filterValueRecord f r@ValueRecord {} = f r 

recordsWithIndex hostname result = zip [1..] $ filter _filter $ result
  where 
    _filter TextRecord {} = False
    _filter ValueRecord {hostName=hostNameValue} = elem hostname hostNameValue 

view recordsWithIndexToView = map (\v -> showIp v) recordsWithIndexToView
  where 
    showIp v 
      | openValue = (show index) ++ ". " ++ ipValue 
      | otherwise = (show index) ++ ". #" ++ ipValue 
      where 
        r = snd v
        index = fst v
        ipValue = ip r
        openValue = open r

openRecord records hostNameSelected ipValueSelected = map _open records 
  where
    _open r@TextRecord {} = r
    _open r@ValueRecord {}
      | ipValue == ipValueSelected && hostNameMatched = r { open = True }
      | hostNameMatched = r { open = False }
      | otherwise = r
      where
        ipValue = ip r 
        hostNameValue = hostName r
        hostNameMatched = elem hostNameSelected hostNameValue

gopenRecord records groupname = map _open records 
  where
    _open r@TextRecord {} = r
    _open r
      | groupValue == groupname = r { open = True }
      | otherwise = r 
      where
        groupValue = group r 

closeRecord records hostname = map _close records 
  where
    _close r@TextRecord {} = r
    _close r
      | hostNameValue == [hostname] = r { open = False }
      | otherwise = r 
      where
        hostNameValue = hostName r 

gcloseRecord records groupname = map _close records 
  where
    _close r@TextRecord {} = r
    _close r
      | groupValue == groupname = r { open = False }
      | otherwise = r 
      where
        groupValue = group r 
      
printLines = mapM putStrLn 

execute "view" result args = printLines $ view $ records
  where 
    hostname = args !! 1
    allRecords = toRecords $ toLinesGroup result
    records = recordsWithIndex hostname allRecords 

execute "open" result args
  | length args > 2 = do 
      let
        ipValue = args !! 2
        fileContent = getFileContent ipValue 
        laststeps
          | isIp ipValue = do
            putStr $ show $ length fileContent
            putStrLn " Chars"
            printLines ["Try to use " ++ ipValue ++ " for " ++ hostname]
            writeFile "/etc/hosts" fileContent
            printLines ["Complete\n"] 
          | otherwise = do 
            printLines ["Wrong ip format"] 
      laststeps
  | ipCount < 1 = do
    printLines ["no host matched"] 
  | otherwise = do
    putStrLn "Choose ip to use: "
    printLines $ view $ records
    putStrLn ""
    putStrLn "Enter line number: "
    num <- getLine
    putStrLn ""
    let 
      selected = filter (\v -> (show $ fst v) == num) records 
      openSelected = map snd selected 
      ipValue = ip $ openSelected !! 0
      fileContent = getFileContent ipValue 
      laststeps
        | (length openSelected) > 0 = do
          printLines ["Try to use " ++ ipValue ++ " for " ++ hostname]
          writeFile "/etc/hosts" fileContent
          printLines ["Complete\n"] 
        | otherwise = do 
          printLines ["Wrong line number\n"] 
    laststeps
  where 
    hostname = args !! 1
    allRecords = toRecords $ toLinesGroup result
    records = recordsWithIndex hostname allRecords 
    ipCount = length $ view $ records
    getFileContent = toHostFile . (zip [0..]) . (openRecord allRecords hostname)

execute "gopen" result args 
  | (length matchRecords) < 1 = do 
    printLines ["no host matched"] 
  | otherwise = do
    putStr $ show $ length fileContent
    putStrLn " Chars"
    writeFile "/etc/hosts" fileContent
    printLines ["completed"] 
  where 
    groupname = args !! 1
    allRecords = toRecords $ toLinesGroup result
    matchRecords = filter (filterValueRecord (\r -> (group r) == groupname)) allRecords 
    fileContent = toHostFile $ (zip [0..]) $ gopenRecord allRecords groupname 

execute "close" result args = do 
  putStr $ show $ length fileContent
  putStrLn " Chars"
  writeFile "/etc/hosts" fileContent
  printLines ["completed"] 
  where 
    hostname = args !! 1
    allRecords = toRecords $ toLinesGroup result
    fileContent = toHostFile $ (zip [0..]) $ closeRecord allRecords hostname 

execute "gclose" result args
  | (length matchRecords) < 1 = do 
    printLines ["no host matched"] 
  | otherwise = do
    putStr $ show $ length fileContent
    putStrLn " Chars"
    writeFile "/etc/hosts" fileContent
    printLines ["completed"] 
  where 
    groupname = args !! 1
    allRecords = toRecords $ toLinesGroup result
    matchRecords = filter (filterValueRecord (\r -> (group r) == groupname)) allRecords 
    fileContent = toHostFile $ (zip [0..]) $ gcloseRecord allRecords groupname 

execute "iplist" result args = do
  printLines [iplist] 
  where 
    allRecords = toRecords $ toLinesGroup result
    iplist = trim $ foldl (makeString ip) "" allRecords 

execute "hostlist" result args = do
  printLines [hostlist] 
  where 
    allRecords = toRecords $ toLinesGroup result
    hostlist = trim $ foldl (makeString((join " ") . hostName)) "" allRecords 

execute "op" result args = do
  printLines ["open gopen close gclose iplist hostlist"] 
execute _ _ _ = printLines ["unknown operation"]

main = do 
  args <- getArgs
  contents <- readFile "/etc/hosts"
  let 
    operation = args !! 0
    result = split "\n" contents
  execute operation result args 
