module Main where

-- ansi-terminal
-- haskeline

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import System.Console.ANSI
import Data.Maybe
import System.Console.Haskeline
import System.IO.Error
import System.Directory
import System.Posix.Directory
import System.Posix.IO
import System.Posix.Process
import System.Posix.Signals

grouping :: (a -> Bool) -> [a] -> [[a]]
grouping _ [] = []
grouping f xs =
  let (ys, zs) = break f xs
  in ys : grouping f (dropWhile f zs)

parsePipeline :: [String] -> [[String]]
parsePipeline = grouping (== "|")

runProcess :: [String] -> IO ()
runProcess (x : xs) = do
  installHandler sigINT Default Nothing
  executeFile x True xs Nothing
runProcess _ = return ()

runLine :: [[String]] -> IO ()
runLine (xs : ys : zs) = do
  when (not (null zs)) (putStr "TODO: " >> print zs)
  (xp, yp) <- createPipe
  pid1 <- forkProcess (dupTo yp stdOutput >> closeFd xp >> runProcess xs)
  pid2 <- forkProcess (dupTo xp stdInput >> closeFd yp >> runProcess ys)
  closeFd xp
  closeFd yp
  void $ getProcessStatus True False pid1
  void $ getProcessStatus True False pid2
runLine (xs : []) = do
  pid <- forkProcess (runProcess xs)
  void $ getProcessStatus True False pid
runLine _ = return ()

reset :: [SGR] -> String -> String
reset codes s = setSGRCode codes ++ s ++ setSGRCode [Reset]

promptTitle :: IO String
promptTitle = do
  path <- getWorkingDirectory
  return $ reset [SetColor Foreground Vivid Red] path ++ " " ++ reset [SetColor Foreground Vivid Cyan] "$ "

main :: IO ()
main = runInputT defaultSettings prompt
  where
    prompt = handleInterrupt prompt . withInterrupt $ do
      title <- liftIO $ promptTitle
      s <- getInputLine title
      case fmap (parsePipeline . words) s of
        Nothing -> return ()
        Just (("cd" : ps) : ys) -> do
         when (not (null ys)) (outputStr "TODO: " >> outputStrLn (show ys))
         home <- liftIO getHomeDirectory
         liftIO $ changeWorkingDirectory (fromMaybe home $ listToMaybe ps)
         prompt
        Just xs -> liftIO (runLine xs) >> prompt
