module Chat where

import Network.Socket
import System
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Data.Time 
import Char (isDigit) 

type Message = (Int, String)

--Creates a socket and listens on TCP 6667. 
main :: IO ()
main = do
    chan <- newChan
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet 6667 iNADDR_ANY)
    listen sock 2
    forkIO (forever (do
        (_, msg) <- readChan chan
        return()))
    mainLoop sock chan 0
 
--Manages connections concurrently.  
mainLoop :: Socket -> Chan Message -> Int -> IO ()
mainLoop sock chan n = do
    conn <- accept sock
    forkIO (runConn conn chan n)
    mainLoop sock chan $! n+1

--Input/Output of messages. 
--The duplicated channel acts as a broadcast channel.
--ID numbers based on the order they connect.
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
          "$\\?" -> help n hdl chan 
          ('$':'\\':_) -> quit reader n hdl chan 
          _  -> chat n line chan

--Allows user to check how long they have been in room
--using the command $\SinceLogOn. 
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

--Allows user to check current time
--using the command $\Time
time :: UTCTime -> Handle -> IO()
time now hdl = do
  hPutStrLn hdl (show now)

--Allows users to trash talk each other behind each others 
--backs using the command $\sn secret message
--where n is the userID number who the message is hidden from. 
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

--Allows user to see a list of commands. 
help :: Int -> Handle -> Chan Message -> IO()
help n hdl chan = do
  hPutStrLn hdl "--Commands available from the prompt.\n\nTime\t\tChecks current time\nSinceLogOn\tChecks time since log on\nsn\t\tSends a message hidden from user with ID=n\n?\t\tPrints list of commands\n_\t\tAll other commands quit the chat room"

--Allows the user to leave the chat room with any
--unused command starting with $\. 
quit :: ThreadId -> Int -> Handle -> Chan Message -> IO()
quit reader n hdl chan = do 
  hPutStrLn hdl "We'll miss you."
  killThread reader
  writeChan chan (n, "User " ++ show n ++ " left.")
  hClose hdl
  exitWith ExitSuccess

--Messages are sent to all other clients. 
chat :: Int -> String -> Chan Message -> IO()
chat n line chan = do  
  writeChan chan  (n, "UserID" ++ show n ++ ": " ++ line)

