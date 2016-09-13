module Main where

import System.Directory (getCurrentDirectory)
import System.FilePath (pathSeparator)
import Data.Time
import Data.Time.Format as TimeFormat
import qualified Text.Regex as RegExp
import Data.Time.Clock (UTCTime(..))
import Data.Time.LocalTime
import System.IO (openFile, FilePath, hPutStr, hGetContents, hClose, IOMode(AppendMode,ReadMode))

inputFileName = "SorcererAndTheWhiteSnake_Input.info.srt"
outputFileName = "SorcererAndTheWhiteSnake_Output.info.srt"
shiftInSeconds = -7

main :: IO ()
main = do
    inputFileFullPath <- filePathInCurrentDir inputFileName
    blocks <- (myRegexSplit "\n\n") `fmap` readFile inputFileFullPath
    putStrLn $ "Number of blocks: " ++ (show . length) blocks

    shiftEachBlock blocks

filePathInCurrentDir :: String -> IO String
filePathInCurrentDir fileName = do
    currDirPath <- getCurrentDirectory
    return $ currDirPath ++ (pathSeparator : []) ++ fileName


-- ## Sample block:
-- 1
-- 00:00:42,491 --> 00:00:45,507
-- Master, where did this blizzard come from?
shiftEachBlock :: [String] -> IO ()

shiftEachBlock [] = do putStrLn "Processing done!"

shiftEachBlock (firstBlock : remainingBlocks) =
    let
      (blockNum : blockTime : blockTextArray) = lines firstBlock
      blockTimes = myRegexSplit " --> " blockTime

      (beginTime : endTime : []) = blockTimes
      newBeginTime = transformTime beginTime
      newEndTime = transformTime endTime

      newBlockTime = (newBeginTime ++ " --> " ++ newEndTime)
    in
      do outputFilePath <- filePathInCurrentDir outputFileName
         fileHandle <- openFile outputFilePath AppendMode
         hPutStr fileHandle (blockNum ++ "\n")
         hPutStr fileHandle (newBlockTime ++ "\n")
         hPutStr fileHandle $ unwords blockTextArray ++ "\n\n"
         hClose fileHandle
         shiftEachBlock remainingBlocks

-- 00:06:56,064
transformTime :: String -> String
transformTime aTimeOfBlock =
    let timeParts = myRegexSplit "," aTimeOfBlock
        part1 : part2 : [] = timeParts

        part1UTCTime = TimeFormat.readTime TimeFormat.defaultTimeLocale "%H:%M:%S" part1 :: UTCTime
        newpart1UTCTime = addUTCTime shiftInSeconds part1UTCTime
        newPart1 = TimeFormat.formatTime TimeFormat.defaultTimeLocale "%H:%M:%S" newpart1UTCTime
    in newPart1 ++ "," ++ part2

myRegexSplit :: String -> String -> [String]
myRegexSplit regExp theString =
    let result = RegExp.splitRegex (RegExp.mkRegex regExp) theString
    in filter (not . null) result
