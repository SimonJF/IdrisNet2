module IdrisNet.UDP.UDPClient
-- UDPClient: basically the same as UDPClient but without the need to bind
import Effects
import IdrisNet.Packet
import IdrisNet.PacketLang
import Network.Socket
import IdrisNet.UDP.UDPCommon

%access public

data UDPClient : Effect where
  UDPWriteString :  SocketAddress -> 
                    Port -> 
                    String ->
                    { () }
                    UDPClient (UDPRes ByteLength)

  UDPReadString :   ByteLength ->
                    { () }
                    UDPClient (UDPRes (UDPAddrInfo, String, ByteLength))

  UDPWritePacket :  SocketAddress -> 
                    Port -> 
                    (pl : PacketLang) ->
                    (mkTy pl) ->
                    { () }
                    UDPClient (UDPRes ByteLength)

  UDPReadPacket : (pl : PacketLang) ->
                   Length -> -- As with other occurrences, ideally this would go
                   { () }
                   UDPClient (UDPRes (UDPAddrInfo, Maybe (mkTy pl, ByteLength)))


UDPCLIENT : EFFECT
UDPCLIENT = MkEff () UDPClient

-- Writes a string to the given remote host
udpWriteString : SocketAddress -> Port -> String -> 
                 { [UDPCLIENT] }
                 Eff (UDPRes ByteLength)
udpWriteString sa p s = call (UDPWriteString sa p s)


-- Reads a string, returning the address, data, and length
udpReadString : ByteLength -> 
                { [UDPCLIENT] } 
                Eff (UDPRes (UDPAddrInfo, String, ByteLength))
udpReadString len = call (UDPReadString len)

-- Writes a packet to the given remote host
udpWritePacket : SocketAddress ->
                 Port ->
                 (pl : PacketLang) ->
                 (mkTy pl) ->
                 { [UDPCLIENT] }
                 Eff (UDPRes ByteLength)
udpWritePacket sa p pl pckt = call (UDPWritePacket sa p pl pckt)

-- Reads a packet from the givem remote host
udpReadPacket : (pl : PacketLang) ->
                Length ->
                { [UDPCLIENT] }
                Eff (UDPRes (UDPAddrInfo, Maybe (mkTy pl, ByteLength))) 
udpReadPacket pl len = call (UDPReadPacket pl len)

-- Saves us repeating ourselves in the handler...
withSocket : (a : Type) ->
             (Socket -> IO (UDPRes a)) -> -- socket function
             IO (UDPRes a)
withSocket _ fn = do
  sock_res <- socket AF_INET Datagram 0
  case sock_res of
    Left err => return (UDPFailure err)
    Right sock => fn sock

instance Handler UDPClient IO where
  handle () (UDPWriteString sa p str) k = do
    res <- withSocket ByteLength (\sock => do
      send_res <- sendTo sock sa p str
      case send_res of
        Left err =>
          if err == EAGAIN then
            return $ UDPRecoverableError err
          else
            return $ UDPFailure err
        Right bl => return $ UDPSuccess bl)
    k res ()   

  handle () (UDPReadString bl) k = do
    res <- withSocket (UDPAddrInfo, String, ByteLength) (\sock => do
      recv_res <- recvFrom sock bl
      case recv_res of
        Left err =>
          if err == EAGAIN then
            return (UDPRecoverableError err)   
          else return (UDPFailure err) 
        Right (addr, str, bl) => return $ UDPSuccess (addr, str, bl))
    k res ()

  handle () (UDPWritePacket sa p pl dat) k = do
    res <- withSocket ByteLength (\sock => do
      (pckt, len) <- marshal pl dat
      send_res <- sendToBuf sock sa p pckt len
      case send_res of
           Left err => 
            if err == EAGAIN then
              return (UDPRecoverableError err) 
            else
              return (UDPFailure err)
           Right bl => return (UDPSuccess bl))
    k res ()

  handle () (UDPReadPacket pl len) k = do
    res <- withSocket (UDPAddrInfo, Maybe (mkTy pl, Length)) (\sock => do
      ptr <- sock_alloc len
      recv_res <- recvFromBuf sock ptr len
      case recv_res of
           Left err =>
             if err == EAGAIN then
               return (UDPRecoverableError err) 
             else
               return (UDPFailure err) 
           Right (addr, bl) => do
             res <- unmarshal pl ptr bl
             sock_free ptr
             -- The UDPSuccess depends on the actual network-y
             -- part, not the unmarshalling. If the unmarshalling fails,
             -- we still keep the connection open.
             return (UDPSuccess (addr, res) ))
    k res ()

