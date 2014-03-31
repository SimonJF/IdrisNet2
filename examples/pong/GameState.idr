module GameState

import Network.Socket

import Effect.SDL

PADDLE_DELTA : Int
PADDLE_DELTA = 3 -- How much to change Y-pos by

Width : Type
Width = Int

Height : Type
Height = Int

DEFAULT_WIDTH : Width
DEFAULT_WIDTH = 640

DEFAULT_HEIGHT : Height
DEFAULT_HEIGHT = 480

PADDLE_WIDTH : Width
PADDLE_WIDTH = 50

PADDLE_HEIGHT : Height
PADDLE_HEIGHT = 100

-- Network mode: Either a client, in which case we need the remote 
data PongNetworkMode = PongClient 
                     | PongServer

instance Eq PongNetworkMode where
  PongClient == PongClient = True
  PongServer == PongServer = True
  _ == _ = False


-- Bookkeeping for current game state.
record GameState : Type where
  MkGameState : 
   (pongWidth : Width) -> -- Canvas width
   (pongHeight : Height) -> -- Canvas height
   (pongBallPos : (Int, Int)) -> -- Ball position
   (pongLeftPaddlePos : (Int, Int)) -> -- Position of the left paddle
   (pongRightPaddlePos : (Int, Int)) -> -- Position of the right paddle
   (pongIsUpPressed : Bool) -> -- Is up pressed? If so, decrease Y pos
   (pongIsDownPressed : Bool) -> -- Is down pressed? If so, increase Y pos
   (pongNetworkMode : PongNetworkMode) ->
   (pongAddr : SocketAddress) ->
   (pongPort : Port) ->
   GameState

isServer : GameState -> Bool
isServer st = (pongNetworkMode st) == PongServer

initLPaddlePos : Width -> Height -> (Int, Int)
initLPaddlePos w h = (20, h `div` 2)

initRPaddlePos : Width -> Height -> (Int, Int)
initRPaddlePos w h = (w - (PADDLE_WIDTH + 20), h `div` 2)

newPaddlePos : (Int, Int) -> Bool -> Bool -> Width -> Height -> (Int, Int)
newPaddlePos (x, y) True _ w h = (x, (max (y - PADDLE_DELTA) 0))
newPaddlePos (x, y) _ True w h = (x, (min (y + PADDLE_DELTA) (h - PADDLE_HEIGHT)))
newPaddlePos (x, y) _ _ _ _ = (x, y)

updateLocalPaddlePos : { [STATE GameState] } Eff IO ()
updateLocalPaddlePos = do
  st <- get
  let lpp = pongLeftPaddlePos st
  let rpp = pongRightPaddlePos st
  let w = pongWidth st
  let h = pongHeight st
  let up = pongIsUpPressed st
  let down = pongIsDownPressed st 
  case (pongNetworkMode st) of
      PongServer => put $ record { pongLeftPaddlePos = (newPaddlePos lpp up down w h) } st
      PongClient => put $ record { pongRightPaddlePos = (newPaddlePos rpp up down w h) } st
  


drawPaddles : { [SDL_ON, STATE GameState] } Eff IO ()
drawPaddles = do st <- get
                 drawPaddle (pongLeftPaddlePos st)
                 drawPaddle (pongRightPaddlePos st)
  where drawPaddle : (Int, Int) -> { [SDL_ON] } Eff IO ()
        drawPaddle (x, y) = rectangle white x y PADDLE_WIDTH PADDLE_HEIGHT


initState : Width -> Height -> PongNetworkMode -> SocketAddress -> Port -> GameState 
initState w h = 
  MkGameState w h (0, 0) (initLPaddlePos w h) (initRPaddlePos w h)
              False False
