module Main
-- Main program. Contains main entry point, event loop, and network / concurrency 
-- setup stuff.

import GameState
import GameMessage 

import Network.Socket
import Network.UDP.UDPServer
import Network.UDP.UDPClient

import Effects
import Effect.SDL
import Effect.State
import Effect.StdIO

import Process

import System

CANVAS_WIDTH : Int
CANVAS_WIDTH = 640

CANVAS_HEIGHT : Int
CANVAS_HEIGHT = 480

-- Pong is a type synonym for all the effects we're going to be using within
-- the program.
-- Parameters: 
--  s : SDL Surface, should one exist
--  n : UDP state
--  t : Return type
Pong : Type -> Type -> Type 
Pong s t = { [ SDL s
               , STATE GameState
               , STDIO
               , UDPCLIENT
               , PROCESS (Running GameMessage)] } Eff IO t

PongRunning : Type -> Type
PongRunning t = Pong SDLSurface t

-- TODO: Disambiguation more nicely
networkHandlerThread' : (mthread : ProcPID GameMessage) -> 
                        RunningProcessM GameMessage IO 
                         [UDPSERVER UDPBound, STDIO] 
                         [UDPSERVER (), STDIO] 
networkHandlerThread' pid = with Effects do
  UDPSuccess (_, Just (pckt, _)) <- Network.UDP.UDPServer.udpReadPacket statusUpdate 256
    | UDPSuccess (_, Nothing) => do putStr "Error decoding status packet\n"
                                    networkHandlerThread' pid
    | UDPFailure err => do Network.UDP.UDPServer.udpFinalise 
                           return ()
    | UDPRecoverableError err => do Network.UDP.UDPServer.udpClose 
                                    return ()
  let msg = mkMessage pckt
  sendMessage pid msg
  networkHandlerThread' pid 
 -- spawn : Eff IO 

networkHandlerThread : Port -> 
                       (mthread : ProcPID GameMessage) -> 
                       RunningProcess GameMessage IO [UDPSERVER (), STDIO]
networkHandlerThread p pid = with Effects do
  putStr $ "Binding to port " ++ (show p) ++ "\n"
  UDPSuccess _ <- udpBind Nothing p
    | UDPFailure err => do putStr $ "Error binding: " ++ (show err)  ++ "\n"
                           return ()
    | UDPRecoverableError err => return ()
  networkHandlerThread' pid

handleNetworkEvents : { [STATE GameState, 
                         PROCESS (Running GameMessage)] } Eff IO ()
handleNetworkEvents = when !hasMessage (do
  st <- get
  case !recvMessage of
    (UpdateRemotePaddle r_up r_down x y) => 
      if (isServer st) then
        put ( record { pongRemotePaddleUp = r_up, 
                       pongRemotePaddleDown = r_down,
                       pongRightPaddlePos = (x, y) } st )
      else
        put ( record { pongRemotePaddleUp = r_up,
                       pongRemotePaddleDown = r_down, 
                       pongLeftPaddlePos = (x, y) } st )
    (UpdateRemoteBallPos ball) => put ( record { pongBall = ball } st ))

handleKeyEvents : Maybe Event -> { [STATE GameState] } Eff IO Bool
handleKeyEvents (Just AppQuit) = return False
handleKeyEvents (Just (KeyUp KeyUpArrow)) = do 
  put (record { pongIsUpPressed = False, pongIsPaddleChanged = True } !get)
  return True
handleKeyEvents (Just (KeyDown KeyUpArrow)) = do 
  put (record { pongIsUpPressed = True, pongIsPaddleChanged = True } !get) 
  return True
handleKeyEvents (Just (KeyUp KeyDownArrow)) = do 
  put (record { pongIsDownPressed = False, pongIsPaddleChanged = True } !get)
  return True
handleKeyEvents (Just (KeyDown KeyDownArrow)) = do 
  put (record { pongIsDownPressed = True, pongIsPaddleChanged = True } !get)
  return True
handleKeyEvents (Just (KeyUp KeySpace)) = do
  put (record { pongToLaunch = True } !get)
  return True
handleKeyEvents _ = return True

draw : PongRunning ()
draw = do
  st <- get
  rectangle white 0 0 (pongWidth st) (pongHeight st)
  drawPaddles
  drawBall
  flip 


serverEvents : PongRunning ()
serverEvents = do
  ball_updated <- updateBallServer 
  when ball_updated (sendBallUpdate >>= \_ => return ())

-- 2 peers. Both peers update the other on the paddle positions.
-- Server controls ball position and sends this to the remote user
eventLoop : PongRunning ()
eventLoop = do
  st <- get
  handleNetworkEvents
  continue <- handleKeyEvents !poll
  updateRemotePaddlePos
  paddle_updated <- updateLocalPaddlePos
  when paddle_updated (sendPaddleUpdate >>= \_ => return ())
  if (isServer st) then serverEvents else updateBallPos
  draw
  when continue eventLoop


setupUDP : Port -> PongRunning ()
setupUDP p = with Effects do
  pid <- getPID
  spawn GameMessage (networkHandlerThread p pid) [(), ()]
  putStr "After spawn\n"
  eventLoop

printUsage : IO ()
printUsage = putStrLn "Usage: ./pong <server | client> <local port> <addr> <port>"

readInt : String -> Int
readInt = cast

modeView : String -> Maybe PongNetworkMode
modeView "client" = Just PongClient
modeView "server" = Just PongServer
modeView _ = Nothing

total
parseArgs : List String -> Maybe (PongNetworkMode, Port, SocketAddress, Port)
parseArgs [_, mode, s_lp, s_a, s_p] with (modeView mode, readInt s_lp, parseIPv4 s_a, readInt s_p)
  | (Just nm, l_p, IPv4Addr i1 i2 i3 i4, p) = 
      Just (nm, l_p, (IPv4Addr i1 i2 i3 i4), p)
  | _ = Nothing
parseArgs _ = Nothing


pongMain : Port -> Pong () ()
pongMain p = with Effects do 
              st <- get
              initialise (pongWidth st) (pongHeight st)
              setupUDP p
              eventLoop 
              quit

main : IO ()
main = do
  args <- getArgs
 -- traverse putStrLn args
  case (parseArgs args) of
         (Just (nm, lp, sa, p)) => do
           let i_st = initState CANVAS_WIDTH CANVAS_HEIGHT nm sa p
           runInit [(), i_st, (), (), initProcess] (pongMain lp)
         Nothing => printUsage 




