module Chat where

import Network.Socket
import System.IO
import Control.OldException
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Fix (fix)
import Time 
 
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
    let sendAll msg = writeChan chan (n, msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    hPutStrLn hdl ("You are user number " ++ show n ++ ".")
    sendAll ("User number " ++ show n ++ " entered.")
    logonT <- getClockTime
    chan' <- dupChan chan
    reader <- (forkIO (forever (do
        (n', line) <- readChan chan'
        when (n /= n') $ hPutStrLn hdl line)))
    handle (\_ -> return()) $ fix $ \loop -> do
        line <- liftM init (hGetLine hdl)
        t <- getClockTime 
        case line of
         {-"$\\SinceLogOn" -> do
           hPutStrLn hdl (liftM show (liftM2 diffClockTimes t logonT))
           loop-}
         {-"$\\Time" -> do
           hPutStrLn hdl (liftM show getClockTime)
           loop-}
         ('$':'\\':_) -> hPutStrLn hdl "We'll miss you."
         _      -> do
            sendAll ("UserID" ++ show n ++ ": " ++ line)
            loop
    killThread reader
    sendAll ("User " ++ show n ++ " left.")
    hClose hdl

