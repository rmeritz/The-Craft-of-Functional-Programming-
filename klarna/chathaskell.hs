module Chat where

import Network.Socket
import System
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Data.Time 
import Time 
import Char (isDigit) 

type Message = (Int, String)
 
main :: IO ()
main = do
    chan <- newChan
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet 1125 iNADDR_ANY)
    listen sock 2
    forkIO (forever (do
        (_, msg) <- readChan chan
        return()))
    mainLoop sock chan 0
 
mainLoop :: Socket -> Chan Message -> Int -> IO ()
mainLoop sock chan n = do
    conn <- accept sock
    forkIO (runConn conn chan n)
    mainLoop sock chan $! n+1
 
runConn :: (Socket, SockAddr) -> Chan Message -> Int -> IO ()
runConn (sock, _) chan n = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    hPutStrLn hdl ("You are user number " ++ show n ++ ".")
    writeChan chan (n, "User number " ++ show n ++ " entered.") 
    logOnT <- getCurrentTime
    chan' <- dupChan chan
    reader <- forkIO $ forever $ do
        (n', line) <- readChan chan'
        when (n /= n') $ hPutStrLn hdl line
    handle (\e -> exitWith e) $ forever $ do
        line <- liftM init (hGetLine hdl)
        now <- getCurrentTime
        case line of
          "$\\SinceLogOn" -> timeSinceLogOn logOnT now hdl 
          "$\\Time" -> time now hdl
          ('$':'\\':'s':x) -> secretMessage x n chan' hdl 
          ('$':'\\':_) -> quit reader n hdl chan 
          _  -> chat n line chan

timeSinceLogOn :: UTCTime -> UTCTime -> Handle -> IO() 
timeSinceLogOn logOnT now hdl = do 
  let diff = diffUTCTime now logOnT
  hPutStrLn hdl (timeChattingMsg diff)

timeChattingMsg :: NominalDiffTime -> String 
timeChattingMsg diff 
  | diff < 0 = "Are you traveling through time?"
  | diff > 86400 =  "You've been online "
    ++ show diff ++".\n You need some fresh air."
  | otherwise = "You have been chatting for " ++ show diff ++ "."

time :: UTCTime -> Handle -> IO()
time now hdl = do
  hPutStrLn hdl (show now)
  
secretMessage :: String -> Int -> Chan Message -> Handle -> IO()
secretMessage x n chan' hdl = do 
  let sIDNum = takeWhile isDigit x
  let sMsg = dropWhile (/=' ') x
  let sHead = "Keep this a secret from " ++ sIDNum ++ ":"
  let s = sHead ++ sMsg
  case sIDNum of
    "" -> do
      hPutStrLn hdl "You didn't choose whose back to talk behind."
    _ -> do
      let sn = (read sIDNum :: Int)
      when (sn /= n) $ writeChan chan' (sn, s)

quit :: ThreadId -> Int -> Handle -> Chan Message -> IO()
quit reader n hdl chan = do 
  hPutStrLn hdl "We'll miss you."
  killThread reader
  writeChan chan (n, "User " ++ show n ++ " left.")
  hClose hdl
  exitWith ExitSuccess

chat :: Int -> String -> Chan Message -> IO()
chat n line chan = do  
  writeChan chan  (n, "UserID" ++ show n ++ ": " ++ line)

