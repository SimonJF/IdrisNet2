module IdrisNet.UDP.UDPServer
import Effects
import IdrisNet.Packet
import IdrisNet.PacketLang
import Network.Socket
import IdrisNet.UDP.UDPCommon
%access public

{- UDP server sockets need to *bind* to a port in order to receive,
 - but don't need to listen or accept. This really simplifies the 
 - state machine :)
-}

-- Bound State
data UDPBound : Type where 
  UDPB : Socket -> UDPBound

-- Error State
data UDPError : Type where
  UDPE : Socket -> UDPError

{- Transition Functions -}
interpUDPBindRes : UDPRes a -> Type
interpUDPBindRes (UDPSuccess _) = UDPBound
interpUDPBindRes _ = ()

interpUDPOperationRes : UDPRes a -> Type
interpUDPOperationRes (UDPSuccess _ ) = UDPBound
interpUDPOperationRes (UDPFailure _ ) = UDPError
interpUDPOperationRes (UDPRecoverableError _) = UDPBound


{- UDP Server Effect -}
data UDPServer : Effect where
  UDPSBind : (Maybe SocketAddress) -> 
             Port -> 
             { () ==> interpUDPBindRes result } 
             UDPServer (UDPRes ())

  UDPSClose : { UDPBound ==> ()} UDPServer ()

  UDPSWriteString : SocketAddress ->
                    Port ->
                    String ->
                    { UDPBound ==> interpUDPOperationRes result}
                    UDPServer (UDPRes ByteLength)

  UDPSReadString :  ByteLength ->
                    { UDPBound ==> interpUDPOperationRes result}
                    UDPServer (UDPRes (UDPAddrInfo, String, ByteLength))

  UDPSWritePacket : SocketAddress ->
                    Port -> 
                    (pl : PacketLang) ->
                    (mkTy pl) ->
                    { UDPBound ==> interpUDPOperationRes result}
                    UDPServer (UDPRes ByteLength)

  UDPSReadPacket : (pl : PacketLang) ->
                   Length ->
                   { UDPBound ==> interpUDPOperationRes result}
                   UDPServer (UDPRes (UDPAddrInfo, Maybe (mkTy pl, ByteLength)))

  UDPSReadPacketBuf : (pl : PacketLang) ->
                   Length -> 
                   { UDPBound ==> interpUDPOperationRes result}
                   UDPServer (UDPRes (UDPAddrInfo, Maybe (mkTy pl, ByteLength, BufPtr)))

  UDPSFinalise : { UDPError ==> () } UDPServer ()


UDPSERVER : Type -> EFFECT
UDPSERVER t = MkEff t UDPServer

-- Binds to a socket address (if not given, any available one) and a port
udpBind : (Maybe SocketAddress) -> 
          Port -> 
          { [UDPSERVER ()] ==> [UDPSERVER (interpUDPBindRes result)] }
          Eff (UDPRes ())
udpBind sa p = call (UDPSBind sa p)

-- Closes a listening socket
udpClose : { [UDPSERVER UDPBound] ==> [UDPSERVER ()] } Eff ()
udpClose = call UDPSClose

-- Writes a string to the given remote host
udpWriteString : SocketAddress -> 
                 Port ->
                 String -> 
                 { [UDPSERVER UDPBound] ==> [UDPSERVER (interpUDPOperationRes result)]}
                 Eff (UDPRes ByteLength)
udpWriteString sa p s = call (UDPSWriteString sa p s)

-- Receives a string
udpReadString : ByteLength -> 
                { [UDPSERVER UDPBound] ==> [UDPSERVER (interpUDPOperationRes result)]} 
                Eff (UDPRes (UDPAddrInfo, String, ByteLength))
udpReadString len = call (UDPSReadString len)

-- Writes a PacketLang packet
udpWritePacket : SocketAddress -> 
                 Port ->
                 (pl : PacketLang) ->
                 (mkTy pl) ->
                 { [UDPSERVER UDPBound] ==> [UDPSERVER (interpUDPOperationRes result)]}
                 Eff (UDPRes ByteLength)
udpWritePacket sa p pl pckt = call (UDPSWritePacket sa p pl pckt)

-- Reads a PacketLang packet
udpReadPacket : (pl : PacketLang) ->
                Length ->
                { [UDPSERVER UDPBound] ==> [UDPSERVER (interpUDPOperationRes result)]}
                Eff (UDPRes (UDPAddrInfo, Maybe (mkTy pl, ByteLength))) 
udpReadPacket pl len = call (UDPSReadPacket pl len)

-- Reads a PacketLang packet but retains the buffer
udpReadPacket' : (pl : PacketLang) ->
                Length ->
                { [UDPSERVER UDPBound] ==> [UDPSERVER (interpUDPOperationRes result)]}
                Eff (UDPRes (UDPAddrInfo, Maybe (mkTy pl, ByteLength, BufPtr))) 
udpReadPacket' pl len = call (UDPSReadPacketBuf pl len)

udpFinalise : { [UDPSERVER UDPError] ==> [UDPSERVER ()]} Eff ()
udpFinalise = call UDPSFinalise

instance Handler UDPServer IO where
  handle () (UDPSBind sa p) k = do
    sock_res <- socket AF_INET Datagram 0
    case sock_res of
      Left err => k (UDPFailure err) ()
      Right sock => do
        bind_res <- bind sock sa p
        if bind_res == 0 then
          k (UDPSuccess ()) (UDPB sock)
        else do
          close sock
          k (UDPFailure bind_res) ()

  handle (UDPB sock) (UDPSClose) k = do
    close sock
    k () ()

  handle (UDPE sock) (UDPSFinalise) k = do
    close sock
    k () ()

  handle (UDPB sock) (UDPSWriteString sa p str) k = do
    send_res <- sendTo sock sa p str
    case send_res of
      Left err =>
        if err == EAGAIN then
          k (UDPRecoverableError err) (UDPB sock)
        else
          k (UDPFailure err) (UDPE sock)
      Right bl => k (UDPSuccess bl) (UDPB sock)
         

  handle (UDPB sock) (UDPSReadString bl) k = do
    recv_res <- recvFrom sock bl
    case recv_res of
      Left err =>
        if err == EAGAIN then
          k (UDPRecoverableError err) (UDPB sock)  
        else k (UDPFailure err) (UDPE sock)
      Right (addr, str, bl) => k (UDPSuccess (addr, str, bl)) (UDPB sock)

  handle (UDPB sock) (UDPSWritePacket sa p pl dat) k = do
    (pckt, len) <- marshal pl dat
    dumpPacket pckt (len * 8)
    send_res <- sendToBuf sock sa p pckt len
    case send_res of
         Left err => 
          if err == EAGAIN then
            k (UDPRecoverableError err) (UDPB sock)
          else
            k (UDPFailure err) (UDPE sock)
         Right bl => k (UDPSuccess bl) (UDPB sock)

  handle (UDPB sock) (UDPSReadPacket pl len) k = do
    ptr <- sock_alloc len
    recv_res <- recvFromBuf sock ptr len
    case recv_res of
         Left err =>
           if err == EAGAIN then
             k (UDPRecoverableError err) (UDPB sock)
           else
             k (UDPFailure err) (UDPE sock)
         Right (addr, bl) => do
           res <- unmarshal pl ptr bl
           sock_free ptr
           -- The UDPSuccess depends on the actual network-y
           -- part, not the unmarshalling. If the unmarshalling fails,
           -- we still keep the connection open.
           k (UDPSuccess (addr, res)) (UDPB sock) 

  handle (UDPB sock) (UDPSReadPacketBuf pl len) k = do
    ptr <- sock_alloc len
    recv_res <- recvFromBuf sock ptr len
    case recv_res of
         Left err =>
           if err == EAGAIN then
             k (UDPRecoverableError err) (UDPB sock)
           else
             k (UDPFailure err) (UDPE sock)
         Right (addr, bl) => do
           res <- unmarshal pl ptr bl
           case res of 
             Just (pckt', bl') => do
               k (UDPSuccess (addr, Just (pckt', bl', ptr))) (UDPB sock)
             Nothing => sock_free ptr $> k (UDPSuccess (addr, Nothing)) (UDPB sock)
           -- The UDPSuccess depends on the actual network-y
           -- part, not the unmarshalling. If the unmarshalling fails,
           -- we still keep the connection open.

