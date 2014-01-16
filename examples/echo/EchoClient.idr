module Main
import Effects
import Network.TCP.TCPClient
import Effect.StdIO

mutual
  recvAndPrint : ByteLength -> { [TCPCLIENT (ClientConnected), STDIO] ==> [TCPCLIENT (), STDIO]} EffM IO ()
  recvAndPrint len = do 
    recv_res <- tcpRecv len
    case recv_res of
      OperationSuccess (str, len) => do lift' (putStr ("Received: " ++ str ++ "\n"))
                                        getAndSend
      RecoverableError _ => recvAndPrint len
      FatalError _       => tcpFinalise
      ConnectionClosed   => return ()


  getAndSend : { [TCPCLIENT (ClientConnected), STDIO] ==> [TCPCLIENT (), STDIO]} EffM IO ()
  getAndSend = do
    input <- getStr
    if (input == "bye!\n") then 
       (lift' (do tcpClose 
                  Effects.return ())) 
    else (do
      tcp_res <- tcpSend input
      case tcp_res of
           OperationSuccess len => recvAndPrint 1024
           RecoverableError _ => getAndSend
           FatalError _       => do lift' tcpFinalise
                                    return ()
           ConnectionClosed => return ())

echoClient : SocketAddress -> Port -> { [TCPCLIENT (), STDIO] ==> [TCPCLIENT (), STDIO]} EffM IO ()
echoClient sa port = do
  connect_res <- tcpConnect sa port
  case connect_res of 
       OperationSuccess sock => do putStr "Connected!\n"
                                   getAndSend
       RecoverableError _ => echoClient sa port
       ConnectionClosed => do putStr "Unable to connect :(\n"
                              return ()
       FatalError _ => do putStr "Unable to connect :( \n"
                          return ()

main : IO ()
main = run [(), ()] (echoClient (IPv4Addr 127 0 0 1) 1234)
