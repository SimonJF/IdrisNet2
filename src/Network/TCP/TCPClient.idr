-- Network.TCP.TCPClient: TCP Client
module Network.TCP.TCPClient
import Network.PacketLang
import Network.Packet
import Network.TCP.TCPCommon
import Network.Socket
import Effects

%access public

data ClientConnected : Type where
  CC : Socket -> ClientConnected

-- 
data ErrorState : Type where
  ES : Socket -> ErrorState

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
  Connect      : SocketAddress -> Port -> {() ==> interpConnectRes result}
                                          TCPClient (SocketOperationRes Socket)
  Close        : { ClientConnected ==> ()} TCPClient () 
  Finalise     : { ErrorState ==> () } TCPClient () 
  WriteString  : String -> { ClientConnected ==> interpOperationRes result}
                           TCPClient (SocketOperationRes ByteLength) 
  ReadString   : ByteLength -> 
                 { ClientConnected ==> interpOperationRes result }
                 TCPClient (SocketOperationRes (String, ByteLength))

  WritePacket  : (pl : PacketLang) ->
                 (mkTy pl) ->
                 { ClientConnected ==> interpOperationRes result }
                 TCPClient (SocketOperationRes ByteLength)

  ReadPacket   : (pl : PacketLang) ->
                 Length -> -- I really dislike this sod being here
                 { ClientConnected ==> interpOperationRes result }
                 TCPClient (SocketOperationRes (mkTy pl, ByteLength))
                 


TCPCLIENT : Type -> EFFECT
TCPCLIENT t = MkEff t TCPClient


tcpConnect : SocketAddress -> Port -> { [TCPCLIENT ()] ==> [TCPCLIENT (interpConnectRes result)] }
                                      Eff IO (SocketOperationRes Socket) 
tcpConnect sa port = (Connect sa port)

tcpClose : { [TCPCLIENT (ClientConnected)] ==> [TCPCLIENT ()] } Eff IO ()
tcpClose = Close

tcpFinalise : { [TCPCLIENT (ErrorState)] ==> [TCPCLIENT ()] } Eff IO () 
tcpFinalise = Finalise

tcpSend : String -> { [TCPCLIENT (ClientConnected)] ==> 
                      [TCPCLIENT (interpOperationRes result)] }
                     Eff IO (SocketOperationRes ByteLength)
tcpSend dat = (WriteString dat)

tcpRecv : ByteLength -> 
          { [TCPCLIENT (ClientConnected)] ==> 
            [TCPCLIENT (interpOperationRes result)] } 
          Eff IO (SocketOperationRes (String, ByteLength))
tcpRecv bl = (ReadString bl)

tcpWritePacket : (pl : PacketLang) ->
                 (mkTy pl) ->
                 { [TCPCLIENT ClientConnected] ==> 
                   [TCPCLIENT (interpOperationRes result) }
                 Eff IO (SocketOperationRes ByteLength)
tcpWritePacket pl dat = (WritePacket pl dat)

tcpReadPacket : (pl : PacketLang) ->
                Length -> -- TODO: Ideally we won't need this parameter
                { ClientConnected ==> interpOperationRes result }
                Eff IO (SocketOperationRes (Maybe (mkTy pl, ByteLength)) }
tcpReadPacket pl len = (ReadPacket pl len)

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
           else do
             close sock
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
                               if (err == 0) then -- socket closed
                                 k (ConnectionClosed) ()
                               else
                                 k (FatalError err) (ES sock))
                    (\res => k (OperationSuccess res) (CC sock))

