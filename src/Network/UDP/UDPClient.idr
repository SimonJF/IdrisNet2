module Network.UDP.UDPClient
-- UDPClient: basically the same as UDPClient but without the need to bind
import Effects
import Network.Packet
import Network.PacketLang
import Network.Socket
import Network.UDP.UDPCommon

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

udpWriteString : SocketAddress -> Port -> String -> 
                 { [UDPCLIENT] }
                 Eff IO (UDPRes ByteLength)
udpWriteString sa p s = (UDPWriteString sa p s)

udpReadString : ByteLength -> 
                { [UDPCLIENT] } 
                Eff IO (UDPRes (UDPAddrInfo, String, ByteLength))
udpReadString len = (UDPReadString len)

udpWritePacket : SocketAddress ->
                 Port ->
                 (pl : PacketLang) ->
                 (mkTy pl) ->
                 { [UDPCLIENT] }
                 Eff IO (UDPRes ByteLength)
udpWritePacket sa p pl pckt = (UDPWritePacket sa p pl pckt)

udpReadPacket : (pl : PacketLang) ->
                Length ->
                { [UDPCLIENT] }
                Eff IO (UDPRes (UDPAddrInfo, Maybe (mkTy pl, ByteLength))) 
udpReadPacket pl len = (UDPReadPacket pl len)


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

