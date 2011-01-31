import Network.Socket
import Time

main :: IO ()
main = do
    -- create socket
    sock <- socket AF_INET Stream 0
    -- make socket immediately reusable - eases debugging.
    setSocketOption sock ReuseAddr 1
    -- listen on TCP port 4242
    bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
    -- allow a maximum of 2 outstanding connections
    listen sock 2
    mainLoop sock
 
mainLoop :: Socket -> IO ()
mainLoop sock = do
    -- accept one connection and handle it
    conn <- accept sock
    runConn conn
    mainLoop sock
 
runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    t <- getClockTime
		send sock "Hi!\n"
		return t 
    sClose sock

		
