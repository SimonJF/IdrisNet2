-- Network.TCP.TCPClient: TCP Client
module Network.TCP.TCPClient
import Network.Socket
import Effects

data ClientConnected : Type where
  CC : Socket -> ClientConnected

-- 
data ErrorState : Type where
  ES : Socket -> ErrorState

data SocketOperationRes a = OperationSuccess a
                          | FatalError SocketError -- Most socket errors are fatal.
                          | RecoverableError SocketError -- EAGAIN / EWOULDBLOCK
                          | ConnectionClosed

-- Interprets the result of 
interpConnectRes : SocketOperationRes a -> Type
interpConnectRes (OperationSuccess _) = ClientConnected 
interpConnectRes _ = ()

-- Interprets the result of a socket operation (for example reading, writing or binding
interpOperationRes : SocketOperationRes a -> Type
interpOperationRes (OperationSuccess _) = ClientConnected
interpOperationRes (FatalError _) = ErrorState
interpOperationRes (RecoverableError _) = ClientConnected
interpOperationRes ConnectionClosed = ()

-- TCP Client Effect
data TCPClient : Effect where
  Connect      : SocketAddress -> Port -> TCPClient (SocketOperationRes Socket) 
                                                    () (interpConnectRes)
  Close        : TCPClient () (ClientConnected) (const ())
  Finalise     : TCPClient () (ErrorState) (const ())
  WriteString  : String -> TCPClient (SocketOperationRes ByteLength) 
                                     (ClientConnected) 
                                     (interpOperationRes)
  ReadString   : ByteLength -> 
                 TCPClient (SocketOperationRes (String, ByteLength)) 
                           (ClientConnected) 
                           (interpOperationRes)

-- Would be nice to pull these in from C...
EAGAIN : Int 
EAGAIN = 11

TCPCLIENT : Type -> EFFECT
TCPCLIENT t = MkEff t TCPClient


tcpConnect : SocketAddress -> Port -> EffM IO (SocketOperationRes Socket) 
                                              [TCPCLIENT ()] 
                                              (\x => [TCPCLIENT (interpConnectRes x)])
tcpConnect sa port = (Connect sa port)

tcpClose : EffM IO () [TCPCLIENT (ClientConnected)] (\_ => [TCPCLIENT ()])
tcpClose = Close

tcpFinalise : EffM IO () [TCPCLIENT (ErrorState)] (\_ => [TCPCLIENT ()])
tcpFinalise = Finalise

tcpSend : String -> EffM IO (SocketOperationRes ByteLength)
                            [TCPCLIENT (ClientConnected)]
                            (\x => [TCPCLIENT (interpOperationRes x)])
tcpSend dat = (WriteString dat)

tcpRecv : ByteLength -> EffM IO (SocketOperationRes (String, ByteLength))
                                [TCPCLIENT (ClientConnected)]
                                (\x => [TCPCLIENT (interpOperationRes x)])
tcpRecv bl = (ReadString bl)


instance Handler TCPClient IO where

  handle () (Connect sa port) k = do
    -- Firstly create a socket
    sock_res <- socket AF_INET Stream 0 
    case sock_res of
         Left err => k (FatalError err) ()
         Right sock => do
           conn_res <- connect sock sa port
           if (conn_res == 0) then -- Success
             k (OperationSuccess sock) (CC sock)
           else
             k (FatalError conn_res) ()

  handle (CC sock) (Close) k = do
    close sock
    k () ()

  handle (ES sock) (Finalise) k = do
    close sock
    k () ()

  handle (CC sock) (WriteString dat) k = do
    send_res <- send sock dat
    case send_res of
      Left err => 
      -- Some errors are benign, meaning the connection survives.
        if (err == EAGAIN) then
          k (RecoverableError err) (CC sock)
        else -- Some aren't
          k (FatalError err) (ES sock)
      -- Success: return number of bytes sent
      Right bytes => k (OperationSuccess bytes) (CC sock)

  handle (CC sock) (ReadString byte_len) k = do
    recv_res <- recv sock byte_len
    either recv_res (\err => if (err == EAGAIN) then 
                               k (RecoverableError err) (CC sock)
                             else 
                               k (FatalError err) (ES sock))
                    (\res => k (OperationSuccess res) (CC sock))

