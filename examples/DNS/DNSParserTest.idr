import Effects
import Effect.StdIO
import IdrisNet.UDP.UDPServer
import DNSParser
import DNS
import IdrisNet.Socket 

dnsTest : List DomainFragment ->
          SocketAddress ->
          Port ->
          { [UDPSERVER UDPBound, STDIO, DNSPARSER ()] ==>
            [UDPSERVER (), STDIO, DNSPARSER ()] } Eff IO ()
dnsTest domain remote_host remote_port = with Eff do
  let dns_pckt = mkDNSRequest 1337 domain DNSQTypeA DNSQClassIN 
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

dnsRespTest : List DomainFragment ->
              Port -> 
              SocketAddress -> 
              Port -> 
              { [UDPSERVER (), STDIO, DNSPARSER ()] } Eff IO ()
dnsRespTest domain local_port remote_host remote_port = 
  case !(udpBind Nothing local_port) of
    UDPSuccess _ => dnsTest domain remote_host remote_port
    UDPFailure err => putStr ("Error connecting: " ++ (show err)) 
    UDPRecoverableError err => putStr ("Error connecting: " ++ (show err)) 

getDomainFragment : IO (List DomainFragment)
getDomainFragment = do
  putStrLn "Enter a domain name:"
  map ((split ((==) '.')) . trim) getLine

main : IO ()
main = run (dnsRespTest !getDomainFragment 4099 (IPv4Addr 138 251 206 2) 53) *> main

