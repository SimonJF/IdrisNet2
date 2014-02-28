module Main
import Effects
import Effect.StdIO
import Network.Socket
import Network.TCP.TCPServer


receive' : { [STDIO, TCPSERVERCLIENT ClientConnected] ==>
             [STDIO, TCPSERVERCLIENT ()] } Eff IO ()

echo : String -> { [STDIO, TCPSERVERCLIENT ClientConnected] ==>
             [STDIO, TCPSERVERCLIENT ()] } Eff IO ()



receive' = do
  recv_res <- tcpRecv 1024
  case recv_res of
    OperationSuccess (str, len) => echo str
    RecoverableError _ => receive'
    FatalError err => putStr ("Error receiving: " ++ (show err)) >>= (\_ => finaliseClient)
    ConnectionClosed => return ()

--receive : ClientProgram ()
--echo : String -> ClientProgram ()

echo str = do
  send_res <- tcpSend str
  case send_res of
    OperationSuccess _ => receive'
    RecoverableError _ => echo str
    FatalError err => putStr ("Error sending: " ++ (show err)) >>= (\_ => finaliseClient)
    ConnectionClosed => return ()

receive : ClientProgram ()
receive = new () receive'

forkServerLoop : { [TCPSERVER (ServerListening), STDIO] ==>
               [TCPSERVER (), STDIO] } Eff IO ()
forkServerLoop = do
  -- Accept, and perform the "receive" program with the new socket.
  accept_res <- forkAccept receive
  case accept_res of
       OperationSuccess _ => forkServerLoop
       RecoverableError _ => forkServerLoop
       FatalError err => putStr ("Error accepting: " ++ (show err)) >>= (\_ => finaliseServer)
       ConnectionClosed => return ()

serverLoop : { [TCPSERVER (ServerListening), STDIO] ==>
               [TCPSERVER (), STDIO] } Eff IO ()
serverLoop = do
  -- Accept, and perform the "receive" program with the new socket.
  accept_res <- accept receive
  case accept_res of
       OperationSuccess _ => serverLoop
       RecoverableError _ => serverLoop
       FatalError err => putStr ("Error accepting: " ++ (show err)) >>= (\_ => finaliseServer)
       ConnectionClosed => return ()

setupServer : SocketAddress -> Port -> Bool ->
              { [TCPSERVER (), STDIO] ==>
                [TCPSERVER (), STDIO] } Eff IO ()
setupServer sa port do_fork = do
  putStr "Binding\n" 
  bind_res <- bind sa port
  case bind_res of
    OperationSuccess _ => do
      putStr "Bound\n"
      listen_res <- listen
      case listen_res of
           OperationSuccess _ => do
             putStr "Listening\n"
             if do_fork then forkServerLoop else serverLoop
           RecoverableError err => putStr ("Recoverable error: " ++ (show err)) >>= (\_ => closeBound)
           FatalError err => putStr ("Error binding: " ++ show err) >>= (\_ => finaliseServer)
           ConnectionClosed => return ()
    RecoverableError _ => return ()
    FatalError err => do putStr ("Error binding: " ++ (show err) ++ "\n") 
                         return ()
    ConnectionClosed => return ()


main : IO ()
main = run (setupServer (IPv4Addr 127 0 0 1) 1234 False) 
