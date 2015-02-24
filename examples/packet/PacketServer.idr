module Main

import Effects
import Effect.StdIO

import IdrisNet.Packet
import IdrisNet.PacketLang
import Network.Socket
import IdrisNet.TCP.TCPServer

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
  OperationSuccess _ <- tcpWritePacket simpleResponse simpleResponseInstance
    | RecoverableError _ => sendResponse
    | FatalError err => do putStr ("Fatal error: " ++ (show err)) 
                           tcpFinalise
    | ConnectionClosed => return ()
  putStr "Sent response!\n" 
  tcpClose 


clientTask' : { [STDIO, TCPSERVERCLIENT (ClientConnected)] ==>
                [STDIO, TCPSERVERCLIENT ()] } Eff IO ()
clientTask' = do 
  OperationSuccess m_packet <- tcpReadPacket simpleStruct 1024
    | RecoverableError err => clientTask'
    | FatalError err => do putStr ("Fatal error: " ++ (show err) ++ "\n")
                           tcpFinalise
    | ConnectionClosed => return ()
  case m_packet of
       Just (packet, len) => do
         printSimpleStruct packet
         sendResponse
       Nothing => do 
         putStr "Error decoding packet"
         tcpClose

clientTask : ClientProgram ()
clientTask = new clientTask'

packetServer : Port -> { [TCPSERVER (), STDIO] } Eff IO ()
packetServer p = do
  OperationSuccess _ <- bind Nothing p
    | RecoverableError err => return ()
    | FatalError err => do (putStr $ "Error binding: " ++ (show err))
                           return ()
    | ConnectionClosed => return ()
  OperationSuccess _ <- listen 
    | RecoverableError _ => closeBound
    | FatalError err => do
        putStr ("Error listening: " ++ (show err))
        finaliseServer
    | ConnectionClosed => return ()
  OperationSuccess _ <- accept clientTask
    | RecoverableError _ => closeListening
    | FatalError err => do 
        putStr ("Error accepting client: " ++ (show err))
        finaliseServer
  closeListening

main : IO ()
main = run (packetServer 1234)
