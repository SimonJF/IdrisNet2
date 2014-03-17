module Main
import Effects
import Effect.StdIO
import Network.Socket
import Network.TCP.TCPServer


receive' : { [STDIO, TCPSERVERCLIENT ClientConnected] ==>
             [STDIO, TCPSERVERCLIENT ()] } Eff IO ()
receive' = do
  -- Receive
  OperationSuccess (str, len) <- tcpRecv 1024
    | RecoverableError _ => receive'
    | FatalError err => do putStr ("Error receiving: " ++ (show err)) 
                           tcpFinalise
    | ConnectionClosed => return ()
  -- Echo
  OperationSuccess _ <- tcpSend str
    | RecoverableError err => do putStr ("Error sending: " ++ (show err)) 
                                 tcpClose 
    | FatalError err => do putStr ("Error sending: " ++ (show err)) 
                           tcpFinalise
    | ConnectionClosed => return ()
  receive'

receive : ClientProgram ()
receive = new receive'

forkServerLoop : { [TCPSERVER (ServerListening), STDIO] ==>
               [TCPSERVER (), STDIO] } Eff IO ()
forkServerLoop = do
  -- Accept, and perform the "receive" program with the new socket.
  OperationSuccess _ <- forkAccept receive
       | RecoverableError _ => forkServerLoop
       | FatalError err => do putStr ("Error accepting: " ++ (show err)) 
                              finaliseServer
       | ConnectionClosed => return ()
  forkServerLoop

serverLoop : { [TCPSERVER (ServerListening), STDIO] ==>
               [TCPSERVER (), STDIO] } Eff IO ()
serverLoop = do
  -- Accept, and perform the "receive" program with the new socket.
  OperationSuccess _ <- accept receive
    | RecoverableError _ => serverLoop
    | FatalError err => do putStr ("Error accepting: " ++ (show err)) 
                           finaliseServer
    | ConnectionClosed => return ()
  serverLoop

setupServer : SocketAddress -> Port -> Bool ->
              { [TCPSERVER (), STDIO] } Eff IO ()
setupServer sa port do_fork = do
  putStr "Binding\n" 
  OperationSuccess _ <- bind sa port
    | RecoverableError _ => return ()
    | FatalError err => do putStr ("Error binding: " ++ (show err) ++ "\n") 
                           return ()
    | ConnectionClosed => return ()
  putStr "Bound\n"
  OperationSuccess _ <- listen
    | RecoverableError err => do putStr ("Recoverable error: " ++ (show err)) 
                                 closeBound
    | FatalError err => do putStr ("Error binding: " ++ show err) 
                           finaliseServer
    | ConnectionClosed => return ()
  putStr "Listening\n"
  if do_fork then forkServerLoop else serverLoop


main : IO ()
main = run (setupServer (IPv4Addr 127 0 0 1) 1234 False) 
