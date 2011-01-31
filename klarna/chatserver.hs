module ChatServer where

import Network.Socket
import System.IO 
import Control.OldException
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Fix (fix)
import Time

type Msg = (Int, String) 

data MainOption = Run | Kill 

main ::  IO()
main  = do
	chan <- newChan
	sock <- socket AF_INET Stream 0  
	setSocketOption sock ReuseAddr 1
	bindSocket sock (SockAddrInet 1124 iNADDR_ANY) 
	listen sock 2
	forkIO $ fix $ \loop -> do 
			(_,msg) <- readChan chan
			loop
	mainLoop sock chan 0

mainLoop :: Socket -> Chan Msg -> Int -> IO()
mainLoop sock chan nr = do
	conn <- accept sock
	forkIO (runConn conn chan nr) 
	mainLoop sock chan $! nr+1 

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO()
runConn (sock, _) chan nr = do
	let sendAll msg = writeChan chan (nr, msg) 
	hdl <- socketToHandle sock ReadWriteMode
	hSetBuffering hdl NoBuffering 
--	t <- getClockTime
	hPutStrLn hdl ("You are user number " ++ show nr ++ ".")
	chan' <- dupChan chan
	reader <- forkIO $ fix $ \loop -> do 
		(nr', line) <- readChan chan'
		when (nr /= nr') $ hPutStrLn hdl line
		loop
  handle (\_ -> return()) $ fix $ \loop -> do
		line <- liftM init (hGetLine hdl) 
		case line of
			"$\\" -> hPutStrLn hdl "Terminated"
			_     -> do 
				sendAll (show nr ++ ": " ++ line)
		 		loop
	killThread reader
	sendAll ("User " ++ show nr ++ " left.")
	hClose hdl
{-	
currentTime :: IO String 
currentTime = do
	getClockTime
-}
