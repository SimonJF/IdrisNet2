-- Network.TCP.TCPClient: TCP Client
module IdrisNet.TCP.TCPClient
import IdrisNet.PacketLang
import IdrisNet.Packet
import IdrisNet.TCP.TCPCommon
import Network.Socket
import Effects

%access public

data ClientConnected : Type where
  CC : Socket -> ClientConnected

-- State recording that an error occurred (therefore no operations 
-- may be performed)
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
                 TCPClient (SocketOperationRes (Maybe (mkTy pl, ByteLength)))
                 


TCPCLIENT : Type -> EFFECT
TCPCLIENT t = MkEff t TCPClient

-- Attempts to connect to a remote host and port. If it fails, remains in
-- the uninitialised state. If it succeeds, transitions to ClientConnected.
tcpConnect : SocketAddress -> Port -> { [TCPCLIENT ()] ==> [TCPCLIENT (interpConnectRes result)] }
                                      Eff (SocketOperationRes Socket) 
tcpConnect sa port = call (Connect sa port)

-- Closes a connected socket.
tcpClose : { [TCPCLIENT (ClientConnected)] ==> [TCPCLIENT ()] } Eff ()
tcpClose = call Close

-- Finalises a socket that has errored
tcpFinalise : { [TCPCLIENT (ErrorState)] ==> [TCPCLIENT ()] } Eff () 
tcpFinalise = call Finalise

{- vv These may all fail, so must be checked for failures after the operations -}

-- Sends a string
tcpSend : String -> { [TCPCLIENT (ClientConnected)] ==> 
                      [TCPCLIENT (interpOperationRes result)] }
                     Eff (SocketOperationRes ByteLength)
tcpSend dat = call (WriteString dat)

-- Receives a string
tcpRecv : ByteLength -> 
          { [TCPCLIENT (ClientConnected)] ==> 
            [TCPCLIENT (interpOperationRes result)] } 
          Eff (SocketOperationRes (String, ByteLength))
tcpRecv bl = call (ReadString bl)

-- Sends a PacketLang packet
tcpWritePacket : (pl : PacketLang) ->
                 (mkTy pl) ->
                 { [TCPCLIENT ClientConnected] ==> 
                   [TCPCLIENT (interpOperationRes result)] }
                 Eff (SocketOperationRes ByteLength)
tcpWritePacket pl dat = call (WritePacket pl dat)

-- Receives a PacketLang packet
tcpReadPacket : (pl : PacketLang) ->
                Length -> -- TODO: Ideally we won't need this parameter
                { [TCPCLIENT ClientConnected] ==> [TCPCLIENT (interpOperationRes result)] }
                Eff (SocketOperationRes (Maybe (mkTy pl, ByteLength))) 
tcpReadPacket pl len = call (ReadPacket pl len)

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
    either (\err => if (err == EAGAIN) then 
                               k (RecoverableError err) (CC sock)
                             else 
                               if (err == 0) then -- socket closed
                                 k (ConnectionClosed) ()
                               else
                                 k (FatalError err) (ES sock))
           (\res => k (OperationSuccess res) (CC sock)) recv_res

  handle (CC sock) (WritePacket pl dat) k = do
    (pckt, len) <- marshal pl dat
    send_res <- sendBuf sock pckt len
    case send_res of
         Left err => 
          if err == EAGAIN then
            k (RecoverableError err) (CC sock)
          else
            k (FatalError err) (ES sock)
         Right bl => k (OperationSuccess bl) (CC sock)

  handle (CC sock) (ReadPacket pl len) k = do
    ptr <- sock_alloc len
    recv_res <- recvBuf sock ptr len
    case recv_res of
         Left err =>
           if err == EAGAIN then
             k (RecoverableError err) (CC sock)
           else if err == 0 then
             k (ConnectionClosed) ()
           else
             k (FatalError err) (ES sock)
         Right bl => do
           res <- unmarshal pl ptr bl
           sock_free ptr
           -- The OperationSuccess depends on the actual network-y
           -- part, not the unmarshalling. If the unmarshalling fails,
           -- we still keep the connection open.
           k (OperationSuccess res) (CC sock) 

