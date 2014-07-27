module IdrisNet.TCP.TCPServer
import Effects
import IdrisNet.Packet
import IdrisNet.PacketLang
import IdrisNet.Socket
import IdrisNet.TCP.TCPCommon

%access public

-- Closed -> Listening

{- Server States -}
data ServerBound : Type where
  SB : Socket -> ServerBound

data ServerListening : Type where 
  SL : Socket -> ServerListening

data ServerError : Type where
  SE : Socket -> ServerError

data ClientConnected : Type where
  CC : Socket -> SocketAddress -> ClientConnected

data ClientError : Type where
  CE : Socket -> ClientError

{- Transition Functions -}
interpOperationRes : SocketOperationRes a -> Type
interpOperationRes (OperationSuccess _) = ServerListening
interpOperationRes (FatalError _) = ServerError
interpOperationRes (RecoverableError _) = ServerListening
interpOperationRes (ConnectionClosed) = ()

interpClientOperationRes : SocketOperationRes a -> Type
interpClientOperationRes (OperationSuccess _) = ClientConnected
interpClientOperationRes (FatalError _) = ClientError
interpClientOperationRes (RecoverableError _) = ClientConnected
interpClientOperationRes (ConnectionClosed) = ()

interpListenRes : SocketOperationRes a -> Type
interpListenRes (OperationSuccess _) = ServerListening
interpListenRes ConnectionClosed = ()
interpListenRes (RecoverableError _) = ServerBound
interpListenRes (FatalError _) = ServerError

interpTCPServerBindRes : SocketOperationRes a -> Type
interpTCPServerBindRes (OperationSuccess _) = ServerBound
interpTCPServerBindRes _ = ()

{- This is an effect that deals with an accepted client. We're allowed to 
   read from, write to, and close this socket. 
   An effectual program of type TCPSERVERCLIENT will be runInit, initially in the
   ClientConnected state, upon acceptance of a client. This will be given in the
   form of an argument to Accept. The effectual program must end by closing the socket. -}
data TCPServerClient : Effect where
  WriteString : String -> { ClientConnected ==> interpClientOperationRes result}
                          TCPServerClient (SocketOperationRes ByteLength)
  ReadString : ByteLength -> { ClientConnected ==> interpClientOperationRes result }
                             TCPServerClient (SocketOperationRes (String, ByteLength))
  CloseClient : { ClientConnected ==> () } TCPServerClient () 
  FinaliseClient : { ClientError ==> () }  TCPServerClient () 
  WritePacket  : (pl : PacketLang) ->
                 (mkTy pl) ->
                 { ClientConnected ==> interpClientOperationRes result }
                 TCPServerClient (SocketOperationRes ByteLength)

  ReadPacket   : (pl : PacketLang) ->
                 Length -> -- I really dislike this sod being here
                 { ClientConnected ==> interpClientOperationRes result }
                 TCPServerClient (SocketOperationRes (Maybe (mkTy pl, ByteLength)))


TCPSERVERCLIENT : Type -> EFFECT 
TCPSERVERCLIENT t = MkEff t TCPServerClient

-- Send a string to the accepted client
tcpSend : String -> { [TCPSERVERCLIENT (ClientConnected)] ==> 
                      [TCPSERVERCLIENT (interpClientOperationRes result)] }
                    Eff (SocketOperationRes ByteLength)
tcpSend s = call (WriteString s)

-- Receive a string from the accepted client
tcpRecv : ByteLength -> { [TCPSERVERCLIENT (ClientConnected)] ==> 
                          [TCPSERVERCLIENT (interpClientOperationRes result)] }
                        Eff (SocketOperationRes (String, ByteLength))
tcpRecv bl = call (ReadString bl)

-- Close the connectoin
tcpClose : { [TCPSERVERCLIENT (ClientConnected)] ==> [TCPSERVERCLIENT ()] } Eff ()
tcpClose = call CloseClient

-- Finalise an erroneous connection
tcpFinalise : { [TCPSERVERCLIENT (ClientError)] ==> [TCPSERVERCLIENT ()] } Eff ()
tcpFinalise = call FinaliseClient

-- Write a PacketLang packet
tcpWritePacket : (pl : PacketLang) ->
                 (mkTy pl) ->
                 { [TCPSERVERCLIENT ClientConnected] ==> 
                   [TCPSERVERCLIENT (interpClientOperationRes result)] }
                 Eff (SocketOperationRes ByteLength)
tcpWritePacket pl dat = call (WritePacket pl dat)

-- Read a PacketLang packet
tcpReadPacket : (pl : PacketLang) ->
                Length -> -- TODO: Ideally we won't need this parameter
                { [TCPSERVERCLIENT ClientConnected] ==> 
                  [TCPSERVERCLIENT (interpClientOperationRes result)] }
                Eff (SocketOperationRes (Maybe (mkTy pl, ByteLength))) 
tcpReadPacket pl len = call (ReadPacket pl len)

-- The type of client programs. Starts in ClientConnected, but must end unconnected.
ClientProgram : Type -> Type
ClientProgram t = {[TCPSERVERCLIENT (ClientConnected)] ==> 
                   [TCPSERVERCLIENT ()]} Eff t

instance Handler TCPServerClient IO where
  handle (CC sock addr) (WriteString str) k = do
    send_res <- send sock str
    case send_res of 
      Left err => 
        if (err == EAGAIN) then
          k (RecoverableError err) (CC sock addr)
        else
          k (FatalError err) (CE sock) 
      Right bl => k (OperationSuccess bl) (CC sock addr) 

  handle (CC sock addr) (ReadString bl) k = do
    recv_res <- recv sock bl
    case recv_res of
      Left err =>
        if err == EAGAIN then
          k (RecoverableError err) (CC sock addr)  
        else if err == 0 then do -- Socket closed
          close sock
          k (ConnectionClosed) ()
        else k (FatalError err) (CE sock)
      Right (str, bl) => k (OperationSuccess (str, bl)) (CC sock addr)

  handle (CC sock addr) (CloseClient) k = 
    close sock $> k () ()
  handle (CE sock) (FinaliseClient) k = 
    close sock $> k () ()

  handle (CC sock addr) (WritePacket pl dat) k = do
    (pckt, len) <- marshal pl dat
    send_res <- sendBuf sock pckt len
    case send_res of
         Left err => 
          if err == EAGAIN then
            k (RecoverableError err) (CC sock addr)
          else
            k (FatalError err) (CE sock)
         Right bl => k (OperationSuccess bl) (CC sock addr)

  handle (CC sock addr) (ReadPacket pl len) k = do
    ptr <- sock_alloc len
    recv_res <- recvBuf sock ptr len
    case recv_res of
         Left err =>
           if err == EAGAIN then
             k (RecoverableError err) (CC sock addr)
           else if err == 0 then do
             close sock
             k (ConnectionClosed) ()
           else
             k (FatalError err) (CE sock)
         Right bl => do
           res <- unmarshal pl ptr bl
           sock_free ptr
           -- The OperationSuccess depends on the actual network-y
           -- part, not the unmarshalling. If the unmarshalling fails,
           -- we still keep the connection open.
           k (OperationSuccess res) (CC sock addr) 

data TCPServer : Effect where
  -- TCPServerBind a socket to a given address and port
  TCPServerBind : (Maybe SocketAddress) -> 
                  Port -> 
                  { () ==> interpTCPServerBindRes result }
                   TCPServer (SocketOperationRes ()) 
  -- Listen
  Listen : { ServerBound ==> interpListenRes result } 
           TCPServer (SocketOperationRes ()) 
  -- Accept
  Accept :  ClientProgram t ->
            { ServerListening ==> interpOperationRes result } 
            TCPServer (SocketOperationRes t)
  ForkAccept : ClientProgram () ->
               { ServerListening ==> interpOperationRes result } 
               TCPServer (SocketOperationRes ())
  -- Need separate ones for each. It'd be nice to condense these into 1...
  CloseBound : { ServerBound ==> () } TCPServer () 
  CloseListening : { ServerListening ==> () } TCPServer () 
  Finalise : { ServerError ==> () } TCPServer () 
  

TCPSERVER : Type -> EFFECT
TCPSERVER t = MkEff t TCPServer

{- TCP Accessor Functions -}

-- Binds to a socket address, if given, and a port
bind : (Maybe SocketAddress) -> 
       Port -> { [TCPSERVER ()] ==> 
                 [TCPSERVER (interpTCPServerBindRes result)] } 
                Eff (SocketOperationRes ())
bind sa p = call (TCPServerBind sa p)

-- Listens on a bound socket
listen : { [TCPSERVER (ServerBound)] ==> [TCPSERVER (interpListenRes result)] } 
         Eff (SocketOperationRes ())
listen = call Listen

-- Accepts a new client, and runs the given client program
accept : (ClientProgram t) -> 
         { [TCPSERVER (ServerListening)] ==> [TCPSERVER (interpOperationRes result)] }
         Eff (SocketOperationRes t)
accept prog = call (Accept prog)

-- Accepts in a different thread
forkAccept : (ClientProgram ()) -> 
         { [TCPSERVER (ServerListening)] ==> [TCPSERVER (interpOperationRes result)] }
         Eff (SocketOperationRes ())
forkAccept prog = call (ForkAccept prog)

-- Close a bound server socket
closeBound : { [TCPSERVER (ServerBound)] ==> [TCPSERVER ()] } Eff ()
closeBound = call CloseBound

-- Close a listening server socket
closeListening : { [TCPSERVER (ServerListening)] ==> [TCPSERVER ()] } Eff ()
closeListening = call CloseListening

-- Finalise a server socket that has errored
finaliseServer : { [TCPSERVER (ServerError)] ==> [TCPSERVER ()] } Eff ()
finaliseServer = call Finalise

{- Handler Functions -}
instance Handler TCPServer IO where 
  handle () (TCPServerBind sa p) k = do
    sock_res <- socket AF_INET Stream 0
    case sock_res of
      Left err => k (FatalError err) ()
      Right sock => do
        bind_res <- bind sock sa p
        if bind_res == 0 then
          -- Binding succeeded \o/
          k (OperationSuccess ()) (SB sock)
        else do
          close sock
          k (FatalError bind_res) ()
          
  handle (SB sock) (Listen) k = do
    listen_res <- listen sock
    if listen_res == 0 then
      -- Success
      k (OperationSuccess ()) (SL sock)
    else
      k (FatalError listen_res) (SE sock)

  handle (SL sock) (Accept prog) k = do
    accept_res <- accept sock
    case accept_res of
         Left err => do
           if err == EAGAIN then 
             k (RecoverableError err) (SL sock)
           else 
             k (FatalError err) (SE sock) 
         Right (client_sock, addr) => do
           res <- runInit [(CC client_sock addr)] prog
           k (OperationSuccess res) (SL sock)  

  handle (SL sock) (ForkAccept prog) k = do
    accept_res <- accept sock
    case accept_res of
         Left err => do
           if err == EAGAIN then 
             k (RecoverableError err) (SL sock)
           else 
             k (FatalError err) (SE sock) 
         Right (client_sock, addr) => do
           fork (runInit [(CC client_sock addr)] prog $> return ())
           k (OperationSuccess ()) (SL sock)  

  handle (SB sock) (CloseBound) k = 
    close sock $> k () ()

  handle (SL sock) (CloseListening) k = 
    close sock $> k () ()

  handle (SE sock) (Finalise) k = 
    close sock $> k () ()


