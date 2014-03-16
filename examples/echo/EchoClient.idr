module Main
import Effects
import Network.TCP.TCPClient
import Effect.StdIO

recvAndPrint : ByteLength -> 
               { [TCPCLIENT (ClientConnected), STDIO] ==> 
                 [TCPCLIENT (), STDIO]} 
               Eff IO ()
getAndSend : { [TCPCLIENT (ClientConnected), STDIO] ==> 
               [TCPCLIENT (), STDIO]} 
              Eff IO ()
  
recvAndPrint len = do 
  OperationSuccess (str, len) <- tcpRecv len
    | RecoverableError _ => recvAndPrint len
    | FatalError _ => tcpFinalise
    | ConnectionClosed => return () 
  putStr ("Received: " ++ str ++ "\n")
  getAndSend

getAndSend = do
  input <- getStr
  if (input == "bye!\n") then do 
       tcpClose 
       return ()
  else do
    OperationSuccess len <- tcpSend input
      | RecoverableError _ => getAndSend
      | FatalError _ => tcpFinalise
      | ConnectionClosed => return ()
    recvAndPrint 1024


echoClient : SocketAddress -> 
             Port -> 
             { [TCPCLIENT (), STDIO] ==> 
               [TCPCLIENT (), STDIO]} Eff IO ()
echoClient sa port = do
  OperationSuccess _ <- tcpConnect sa port
    | RecoverableError _ => echoClient sa port
    | ConnectionClosed => putStr "Unable to connect: connection closed. \n"
    | FatalError err => putStr ("Unable to connect: fatal error " ++ (show err))
  putStr "Connected!\n"
  getAndSend


main : IO ()
main = run (echoClient (IPv4Addr 127 0 0 1) 1234)
