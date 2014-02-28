module Main
import Effects
import Effect.StdIO
import Network.Socket
import Network.UDP.UDPClient

getAndSend : Nat -> SocketAddress -> Port ->
            { [UDPCLIENT, STDIO] } Eff IO ()
getAndSend Z sa p = return ()
getAndSend (S k) sa p = do
  putStr $ "Enter a string to send.\n"
  line <- getStr
  case !(udpWriteString sa p line) of 
    UDPSuccess _ => getAndSend k sa p
    UDPRecoverableError _ => getAndSend (S k) sa p
    UDPFailure err => putStr ("Error: " ++ (show err) ++ "\n")


main : IO ()
main = run (getAndSend 5 (IPv4Addr 127 0 0 1) 4099)
