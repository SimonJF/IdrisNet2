module Main
import Effects
import Network.TCP.TCPClient
import Effect.StdIO

clientLoop : ByteLength -> 
               { [TCPCLIENT (ClientConnected), STDIO] ==> 
                 [TCPCLIENT (), STDIO]} 
               Eff IO ()
clientLoop len = do
  input <- getStr
  if (input == "bye!\n") then tcpClose 
  else do
    OperationSuccess _ <- tcpSend input
      | RecoverableError err => do putStr ("Error sending: " ++ (show err))
                                   tcpClose
      | FatalError err => do putStr ("Error sending: " ++ (show err))
                             tcpFinalise
      | ConnectionClosed => return ()  
    OperationSuccess (str, len') <- tcpRecv len
      | RecoverableError err => do putStr ("Error receiving: " ++ (show err))
                                   tcpClose
      | FatalError err => do putStr ("Error receiving: " ++ (show err))
                             tcpFinalise
      | ConnectionClosed => return () 
    putStr ("Received: " ++ str ++ "\n")
    clientLoop len 


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
  clientLoop 1024


main : IO ()
main = run (echoClient (IPv4Addr 127 0 0 1) 1234)
