module Main
import Network.Socket
import Network.TCP.TCPServer

receive : ClientProgram ()
echo : String -> ClientProgram ()

echo str = do
  send_res <- tcpSend str
  case send_res of
    OperationSuccess _ => receive
    RecoverableError _ => echo str
    FatalError _ => finaliseClient
    ConnectionClosed => return ()

receive = do
  recv_res <- tcpRecv 1024
  case recv_res of
    OperationSuccess (str, len) => echo str
    RecoverableError _ => receive
    FatalError _ => finaliseClient
    ConnectionClosed => return ()

forkServerLoop : { [TCPSERVER (ServerListening)] ==>
               [TCPSERVER ()] } Eff IO ()
forkServerLoop = do
  -- Accept, and perform the "receive" program with the new socket.
  accept_res <- forkAccept receive
  case accept_res of
       OperationSuccess _ => forkServerLoop
       RecoverableError _ => forkServerLoop
       FatalError _ => finaliseServer
       ConnectionClosed => return ()

serverLoop : { [TCPSERVER (ServerListening)] ==>
               [TCPSERVER ()] } Eff IO ()
serverLoop = do
  -- Accept, and perform the "receive" program with the new socket.
  accept_res <- accept receive
  case accept_res of
       OperationSuccess _ => serverLoop
       RecoverableError _ => serverLoop
       FatalError _ => finaliseServer
       ConnectionClosed => return ()

setupServer : SocketAddress -> Port -> Bool ->
              { [TCPSERVER ()] ==>
                [TCPSERVER ()] } Eff IO ()
setupServer sa port do_fork = do 
  bind_res <- bind sa port
  case bind_res of
    OperationSuccess _ => do
      listen_res <- listen
      case listen_res of
           OperationSuccess _ => if do_fork then forkServerLoop else serverLoop
           RecoverableError _ => closeBound
           FatalError _ => finaliseServer
           ConnectionClosed => return ()
    RecoverableError _ => return ()
    FatalError _ => return ()
    ConnectionClosed => return ()


main : IO ()
main = runInit [()] (setupServer (IPv4Addr 127 0 0 1) 1234 True) 
