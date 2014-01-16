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

serverLoop : { [TCPSERVER (ServerListening)] ==>
               [TCPSERVER ()] } EffM IO ()
serverLoop = do
  -- Accept, and perform the "receive" program with the new socket.
  accept_res <- accept receive
  case accept_res of
       OperationSuccess _ => serverLoop
       RecoverableError _ => serverLoop
       FatalError _ => finaliseServer
       ConnectionClosed => return ()

setupServer : SocketAddress -> Port -> 
              { [TCPSERVER ()] ==>
                [TCPSERVER ()] } EffM IO ()
setupServer sa port = do 
  bind_res <- bind sa port
  case bind_res of
    OperationSuccess _ => do
      listen_res <- listen
      case listen_res of
           OperationSuccess _ => serverLoop
           RecoverableError _ => closeBound
           FatalError _ => finaliseServer
           ConnectionClosed => return ()
    RecoverableError _ => return ()
    FatalError _ => return ()
    ConnectionClosed => return ()


main : IO ()
main = run [()] (setupServer (IPv4Addr 127 0 0 1) 1234) 
