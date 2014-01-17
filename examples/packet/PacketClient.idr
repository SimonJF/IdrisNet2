module Main

import Effects
import Effect.StdIO

import Network.Packet
import Network.PacketLang
import Network.Socket
import Network.TCP.TCPClient

import PacketStruct

printRecvPacket : (mkTy simpleResponse) -> { [STDIO] } Eff IO ()
printRecvPacket (s1 ## s2) = do
  putStr "Received packet: \n"
  putStr $ "S1: " ++ s1 ++ "\n"
  putStr $ "S2: " ++ s2 ++ "\n"

recvResponse : { [STDIO, TCPCLIENT (ClientConnected)] ==>
                 [STDIO, TCPCLIENT ()] } Eff IO ()
recvResponse = do
  recv_res <- tcpReadPacket simpleResponse 1024
  case recv_res of
      OperationSuccess m_packet =>
        case m_packet of
          Just (pckt, len) => do printRecvPacket pckt
                                 tcpClose
          Nothing => do putStr "Error decoding packet"
                        tcpClose
      RecoverableError err => do lift' (putStr ("Error: " ++ (show err) ++ "\n"))
                                 tcpClose
      FatalError err => do
        lift' (putStr ("Fatal error sending packet: " ++ (show err) ++ "\n"))
        tcpFinalise
      ConnectionClosed => return ()


sendAndRecv : { [STDIO, TCPCLIENT (ClientConnected)] ==>
                [STDIO, TCPCLIENT ()] } Eff IO ()
sendAndRecv = do
  send_res <- tcpWritePacket simpleStruct simpleStructInstance
  case send_res of
       OperationSuccess _ => recvResponse
       RecoverableError err => do lift' (putStr ("Error: " ++ (show err) ++ "\n"))
                                  tcpClose
       FatalError err => do
         lift' (putStr ("Fatal error sending packet: " ++ (show err) ++ "\n"))
         tcpFinalise
       ConnectionClosed => return ()

clientTask : SocketAddress -> Port -> { [STDIO, TCPCLIENT ()] } Eff IO ()
clientTask sa p = do
  connect_res <- tcpConnect sa p
  case connect_res of
       OperationSuccess _ => sendAndRecv
       RecoverableError _ => clientTask sa p
       FatalError err =>
         lift' (putStr ("Fatal error connecting: " ++ (show err) ++ "\n"))
       ConnectionClosed => return ()

main : IO ()
main = run [(), ()] (clientTask (IPv4Addr 127 0 0 1) 1234)

