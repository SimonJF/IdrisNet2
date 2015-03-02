module Main
import Effects
import Effect.StdIO
import Effect.State
import Effect.System
import Network.Socket
import IdrisNet.TCP.TCPServer
import IdrisNet.TCP.TCPCommon


receive : { [TCPSERVERCLIENT ClientConnected, STDIO, SYSTEM] ==>
            [TCPSERVERCLIENT (), STDIO, SYSTEM] } Eff ()
receive = do
  t <- time
  putStr (show t ++ ": Waiting for a message\n")
  -- Receive
  OperationSuccess (str, len) <- tcpRecv 1024
    | RecoverableError _ => receive
    | FatalError err => do putStr ("Error receiving: " ++ (show err))
                           tcpFinalise
    | ConnectionClosed => return ()
  -- Echo
  OperationSuccess _ <- tcpSend $ str
    | RecoverableError err => do putStr ("Error sending: " ++ (show err))
                                 tcpClose 
    | FatalError err => do putStr ("Error sending: " ++ (show err))
                           tcpFinalise
    | ConnectionClosed => return ()
  receive


forkServerLoop : { [STDIO, TCPSERVER (ServerListening)] ==>
               [STDIO, TCPSERVER ()] } Eff ()
forkServerLoop = do
  -- Accept, and perform the "receive" program with the new socket.
  OperationSuccess _ <- forkAccept receive [default, default]
       | RecoverableError _ => forkServerLoop
       | FatalError err => do putStr ("Error accepting: " ++ (show err))
                              finaliseServer
       | ConnectionClosed => return ()
  forkServerLoop

serverLoop : { [STDIO, TCPSERVER (ServerListening)] ==>
               [STDIO, TCPSERVER ()] } Eff ()
serverLoop = do
  -- Accept, and perform the "receive" program with the new socket.
  OperationSuccess _ <- accept receive [default, default]
    | RecoverableError _ => serverLoop
    | FatalError err => do putStr ("Error accepting: " ++ (show err))
                           finaliseServer
    | ConnectionClosed => return ()
  serverLoop

setupServer : Port -> Bool ->
              { [STDIO, TCPSERVER ()] } Eff ()
setupServer port do_fork = do
  putStr "Binding\n" 
  OperationSuccess _ <- bind Nothing port
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
main = run (setupServer 1234 True) 
