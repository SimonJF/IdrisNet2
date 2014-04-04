module Main

import Effects
import Effect.StdIO

import IdrisNet.Packet
import IdrisNet.PacketLang
import IdrisNet.Socket
import IdrisNet.TCP.TCPClient

import PacketStruct

printRecvPacket : (mkTy simpleResponse) -> { [STDIO] } Eff IO ()
printRecvPacket (s1 ## s2) = do
  putStr "Received packet: \n"
  putStr $ "S1: " ++ s1 ++ "\n"
  putStr $ "S2: " ++ s2 ++ "\n"

recvResponse : { [STDIO, TCPCLIENT (ClientConnected)] ==>
                 [STDIO, TCPCLIENT ()] } Eff IO ()
recvResponse = do
  OperationSuccess m_packet <- tcpReadPacket simpleResponse 1024
    | RecoverableError err => do putStr ("Error: " ++ (show err) ++ "\n")
                                 tcpClose
    | FatalError err => do putStr ("Error: " ++ (show err) ++ "\n") 
                           tcpFinalise
    | ConnectionClosed => return ()
  case m_packet of 
    Just (pckt, len) => do printRecvPacket pckt
                           tcpClose
    Nothing => do putStr "Error decoding packet"
                  tcpClose

sendAndRecv : { [STDIO, TCPCLIENT (ClientConnected)] ==>
                [STDIO, TCPCLIENT ()] } Eff IO ()
sendAndRecv = do
  OperationSuccess _ <- tcpWritePacket simpleStruct simpleStructInstance
    | RecoverableError err => do 
         putStr ("Error: " ++ (show err) ++ "\n")
         tcpClose
    | FatalError err => do
         putStr ("Fatal error sending packet: " ++ (show err) ++ "\n")
         tcpFinalise
    | ConnectionClosed => return ()
  recvResponse

clientTask : SocketAddress -> Port -> { [STDIO, TCPCLIENT ()] } Eff IO ()
clientTask sa p = do
  OperationSuccess _ <- tcpConnect sa p
    | RecoverableError _ => clientTask sa p
    | FatalError err =>
         putStr ("Fatal error connecting: " ++ (show err) ++ "\n")
    | ConnectionClosed => return ()
  sendAndRecv

main : IO ()
main = run (clientTask (IPv4Addr 127 0 0 1) 1234)

