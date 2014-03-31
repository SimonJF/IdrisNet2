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
               , PROCESS (Running GameMessage)] } Eff IO t

PongRunning : Type -> Type
PongRunning t = Pong SDLSurface t

-- TODO: Disambiguation more nicely
networkHandlerThread' : (mthread : ProcPID GameMessage) -> 
                        RunningProcessM GameMessage IO 
                         [UDPSERVER UDPBound, STDIO] 
                         [UDPSERVER (), STDIO] 
networkHandlerThread' pid = with Effects do
  putStr "In NHT'\n"
  UDPSuccess (_, dat, _) <- Network.UDP.UDPServer.udpReadString 1024
    | UDPFailure err => do Network.UDP.UDPServer.udpFinalise 
                           return ()
    | UDPRecoverableError err => do Network.UDP.UDPServer.udpClose 
                                    return ()
  networkHandlerThread' pid 
 -- spawn : Eff IO 

networkHandlerThread : SocketAddress -> 
                       Port -> 
                       (mthread : ProcPID GameMessage) -> 
                       RunningProcess GameMessage IO [UDPSERVER (), STDIO]
networkHandlerThread sa p pid = with Effects do
  putStr $ "Binding to port " ++ (show p) ++ "\n"
  UDPSuccess _ <- udpBind sa p
    | UDPFailure err => return ()
    | UDPRecoverableError err => return ()
  putStr "Bound\n"
  networkHandlerThread' pid

handleServerNetworkEvents : { [STATE GameState, 
                         PROCESS (Running GameMessage)] } Eff IO ()
handleServerNetworkEvents = when !hasMessage (do
  st <- get
  case !recvMessage of
    (UpdateRemotePaddle x y) => put ( record { pongRightPaddlePos = (x, y) } st )
    (UpdateRemoteBallPos x y) => put ( record { pongBallPos = (x, y) } st ))

handleKeyEvents : Maybe Event -> { [STATE GameState] } Eff IO ()
handleKeyEvents (Just (KeyUp KeyUpArrow)) = put (record { pongIsUpPressed = False } !get)
handleKeyEvents (Just (KeyDown KeyUpArrow)) = put (record { pongIsUpPressed = True } !get)
handleKeyEvents (Just (KeyUp KeyDownArrow)) = put (record { pongIsDownPressed = False } !get)
handleKeyEvents (Just (KeyDown KeyDownArrow)) = put (record { pongIsDownPressed = True } !get)
handleKeyEvents _ = return ()

draw : PongRunning ()
draw = do
  st <- get
  rectangle black 0 0 (pongWidth st) (pongHeight st)
  drawPaddles
  flip 

serverEventLoop : PongRunning ()
serverEventLoop = do
  putStr "In event loop\n"
  st <- get
  handleServerNetworkEvents
  key_events <- poll
  handleKeyEvents key_events
  updateLocalPaddlePos
  draw
  serverEventLoop

clientEventLoop : PongRunning ()
clientEventLoop = return ()

setupServer : SocketAddress -> Port -> PongRunning ()
setupServer sa p = with Effects do
  pid <- getPID
  spawn GameMessage (networkHandlerThread sa p pid) [(), ()]
  putStr "After spawn\n"
  serverEventLoop

printUsage : IO ()
printUsage = putStrLn "Usage: ./pong <server | client> <addr> <port>"

readInt : String -> Int
readInt = cast

modeView : String -> Maybe PongNetworkMode
modeView "client" = Just PongClient
modeView "server" = Just PongServer
modeView _ = Nothing

total
parseArgs : List String -> Maybe (PongNetworkMode, SocketAddress, Port)
parseArgs [_, mode, s_a, s_p] with (modeView mode, parseIPv4 s_a, readInt s_p)
  | (Just nm, IPv4Addr i1 i2 i3 i4, p) = 
      Just (nm, (IPv4Addr i1 i2 i3 i4), p)
  | _ = Nothing
parseArgs _ = Nothing


pongMain : Pong () ()
pongMain = with Effects do 
              st <- get
              initialise (pongWidth st) (pongHeight st)
              if isServer st then 
                setupServer (pongAddr st) (pongPort st)
              else
                clientEventLoop
              quit

main : IO ()
main = do
  args <- getArgs
 -- traverse putStrLn args
  case (parseArgs args) of
         (Just (nm, sa, p)) => do
           let i_st = initState CANVAS_WIDTH CANVAS_HEIGHT nm sa p
           runInit [(), i_st, (), initProcess] pongMain
         Nothing => printUsage 




