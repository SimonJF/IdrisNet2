import Effects
import Effect.StdIO
import Network.UDP.UDPServer
import DNSParser
import DNS
import Network.Socket 

recvAndParse : { [UDPSERVER UDPBound, STDIO, DNSPARSER ()] ==>
                 [UDPSERVER (), STDIO, DNSPARSER()] } Eff IO ()
recvAndParse = do
  udp_dns <- udpReadPacket' dns 1024
  case udp_dns of
    UDPSuccess (addr, Just (dns_pckt, len, ptr)) => do
      parsed_dns <- parseDNS ptr len dns_pckt
      case parsed_dns of
        Left err => do putStr $ "Error decoding: " ++ show err ++ "\n"
                       udpClose
        Right decoded_dns => do putStr $ "Decoded successfully! " ++ (show decoded_dns)
                                udpClose
    UDPSuccess (addr, Nothing) => do putStr "Error in readPacket... :'(\n" 
                                     udpClose
    UDPFailure err => do putStr $ "Error receiving: " ++ (show err) ++ "\n"
                         udpFinalise
    UDPRecoverableError err => do putStr $ "Error receiving: " ++ (show err) ++ "\n"
                                  udpClose

dnsRespTest : SocketAddress -> Port -> { [UDPSERVER (), STDIO, DNSPARSER ()] } Eff IO ()
dnsRespTest addr port = 
  case !(udpBind addr port) of
    UDPSuccess _ => recvAndParse
    UDPFailure err => putStr ("Error connecting: " ++ (show err)) 
    UDPRecoverableError err => putStr ("Error connecting: " ++ (show err)) 


main : IO ()
main = run (dnsRespTest (IPv4Addr 127 0 0 1) 4099)
