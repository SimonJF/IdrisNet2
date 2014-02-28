module Main

import Effects
import Effect.StdIO

import Network.Packet
import Network.PacketLang
import Network.Socket
import Network.TCP.TCPServer

import PacketStruct

printSimpleStruct : (mkTy simpleStruct) -> { [STDIO] } Eff IO ()
printSimpleStruct (s1 ## s2 ## (Left b_int) ## xs ## b1 ## b2 ## prf) = do
  putStr $ "S1: " ++ s1 ++ "\n"
  putStr $ "S2: " ++ s2 ++ "\n"
  putStr $ "b_int: " ++ (show b_int) ++ "\n"
  putStr $ "xs: " ++ (show xs) ++ "\n"
  putStr $ "b1: " ++ (show b1) ++ "\n"
  putStr $ "b2: " ++ (show b2) ++ "\n"
printSimpleStruct (s1 ## s2 ## (Right s3) ## xs ## b1 ## b2 ## prf) = do
  putStr $ "S1: " ++ s1 ++ "\n"
  putStr $ "S2: " ++ s2 ++ "\n"
  putStr $ "S3: " ++ s2 ++ "\n"
  putStr $ "xs: " ++ (show xs) ++ "\n"
  putStr $ "b1: " ++ (show b1) ++ "\n"
  putStr $ "b2: " ++ (show b2) ++ "\n"

sendResponse : { [STDIO, TCPSERVERCLIENT (ClientConnected)] ==>
                 [STDIO, TCPSERVERCLIENT ()] } Eff IO ()
sendResponse = do 
  send_res <- tcpWritePacket simpleResponse simpleResponseInstance
  case send_res of
    OperationSuccess _ => do putStr "Sent response!\n" 
                             closeClient
    RecoverableError _ => sendResponse
    FatalError err => do putStr ("Fatal error: " ++ (show err)) 
                         finaliseClient
    ConnectionClosed => return ()
  

clientTask' : { [STDIO, TCPSERVERCLIENT (ClientConnected)] ==>
                [STDIO, TCPSERVERCLIENT ()] } Eff IO ()
clientTask' = do 
  op_res <- tcpReadPacket simpleStruct 1024
  case op_res of
    OperationSuccess m_packet => 
      case m_packet of
           Just (packet, len) => do
             printSimpleStruct packet
             sendResponse
           Nothing => do putStr "Error decoding packet"
                         closeClient
    RecoverableError err => clientTask'
    FatalError err => do lift' (putStr $ "Fatal error: " ++ (show err) ++ "\n")
                         finaliseClient
    ConnectionClosed => return ()
      

clientTask : ClientProgram ()
clientTask = new () clientTask'

packetServer : SocketAddress -> Port -> { [TCPSERVER (), STDIO] } Eff IO ()
packetServer sa p = do
  bind_res <- bind sa p
  case bind_res of
       OperationSuccess _ => do
         case !listen of
              OperationSuccess _ => do
                case !(accept clientTask) of
                  OperationSuccess _ => closeListening
                  RecoverableError _ => closeListening
                  FatalError err => do 
                    lift' (putStr $ "Error accepting client: " ++ (show err))
                    finaliseServer
                  ConnectionClosed => return ()
              RecoverableError _ => closeBound
              FatalError err => do 
                lift' (putStr $ "Error listening: " ++ (show err))
                finaliseServer
                return ()
              ConnectionClosed => return ()
       RecoverableError err => return ()
       FatalError err => do lift' (putStr $ "Error binding: " ++ (show err))
                            return ()
       ConnectionClosed => return ()


main : IO ()
main = run (packetServer (IPv4Addr 127 0 0 1) 1234)
