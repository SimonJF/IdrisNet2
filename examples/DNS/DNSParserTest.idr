import Effects
import Effect.StdIO
import Network.UDP.UDPServer
import DNSParser
import DNS
import Network.Socket 

dnsTest : SocketAddress ->
          Port ->
          { [UDPSERVER UDPBound, STDIO, DNSPARSER ()] ==>
            [UDPSERVER (), STDIO, DNSPARSER ()] } Eff IO ()
dnsTest remote_host remote_port = with Eff do
  let dns_pckt = mkDNSRequest 1337 ["simonjf", "com"] DNSQTypeA DNSQClassIN 
  case encodeDNS dns_pckt of
    Left err => with Eff do putStr $ "Error encoding DNS packet... " ++ show err
                            udpClose
    Right pckt => with Eff do
      UDPSuccess w_len <- udpWritePacket remote_host remote_port dns pckt
        | UDPFailure err => with Eff do putStr ("Error sending packet: " ++ (show err))
                                        udpFinalise
        | UDPRecoverableError err => with Eff do putStr ("Error sending packet: " ++ (show err) ++ "\n")
                                                 udpClose
      UDPSuccess (addr, Just (dns_pckt, len, ptr)) <- udpReadPacket' dns 1024 
        | UDPSuccess (addr, Nothing) => with Eff do putStr "Error in readPacket... :'(\n" 
                                                    udpClose
        | UDPFailure err => with Eff do putStr ("Error receiving: " ++ (show err) ++ "\n")
                                        udpFinalise
        | UDPRecoverableError err => with Eff do putStr ("Error receiving: " ++ (show err) ++ "\n")
                                                 udpClose
      parsed_dns <- parseDNS ptr len dns_pckt
      case parsed_dns of
        Left err => with Eff do putStr $ "Error decoding: " ++ show err ++ "\n"
                                udpClose
        Right decoded_dns => with Eff do putStr $ "Decoded successfully! " ++ (show decoded_dns)
                                         udpClose

dnsRespTest : SocketAddress -> 
              Port -> 
              SocketAddress -> 
              Port -> 
              { [UDPSERVER (), STDIO, DNSPARSER ()] } Eff IO ()
dnsRespTest local_addr local_port remote_host remote_port = 
  case !(udpBind local_addr local_port) of
    UDPSuccess _ => dnsTest remote_host remote_port
    UDPFailure err => putStr ("Error connecting: " ++ (show err)) 
    UDPRecoverableError err => putStr ("Error connecting: " ++ (show err)) 


main : IO ()
main = run (dnsRespTest (IPv4Addr 138 251 204 206) 4099 (IPv4Addr 138 251 206 2) 53)

