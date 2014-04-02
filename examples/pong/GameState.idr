module GameState

import Network.Socket

import Effect.SDL
import Effect.State

import Data.Floats

PADDLE_DELTA : Int
PADDLE_DELTA = 2 -- How much to change Y-pos by

Width : Type
Width = Int

Height : Type
Height = Int

DEFAULT_WIDTH : Width
DEFAULT_WIDTH = 640

DEFAULT_HEIGHT : Height
DEFAULT_HEIGHT = 480

PADDLE_WIDTH : Width
PADDLE_WIDTH = 10

PADDLE_HEIGHT : Height
PADDLE_HEIGHT = 90

BALL_DIAMETER : Int
BALL_DIAMETER = 7

LAUNCH_VELOCITY : Int
LAUNCH_VELOCITY = 3

MAX_ANGLE : Float
MAX_ANGLE = (5 * pi) / 12

-- Network mode: Either a client, in which case we need the remote 
data PongNetworkMode = PongClient 
                     | PongServer

instance Eq PongNetworkMode where
  PongClient == PongClient = True
  PongServer == PongServer = True
  _ == _ = False

record PongBall : Type where
  MkPongBall : 
  (pongBallPos : (Int, Int)) ->
  (pongBallXVel : Int) -> -- X Velocity
  (pongBallYVel : Int) -> -- Y Velocity
  (pongBallStuck : Bool) -> -- Whether the ball is attached to a paddle or not
  (pongBallHitLeft : Bool) -> -- Whether the ball last hit the left paddle
  PongBall

instance Show PongBall where
  show (MkPongBall pos xv yv stuck left) = 
    "Pong ball: Position " ++ show pos ++ ", x vel: " ++ 
    show xv ++ ", stuck: " ++ show stuck ++ ", left: " ++ show left

-- Bookkeeping for current game state.
record GameState : Type where
  MkGameState : 
   (pongWidth : Width) -> -- Canvas width
   (pongHeight : Height) -> -- Canvas height
   (pongBall : PongBall) ->
   (pongLeftPaddlePos : (Int, Int)) -> -- Position of the left paddle
   (pongRightPaddlePos : (Int, Int)) -> -- Position of the right paddle
   (pongIsUpPressed : Bool) -> -- Is up pressed? If so, decrease Y pos
   (pongIsDownPressed : Bool) -> -- Is down pressed? If so, increase Y pos
   (pongToLaunch : Bool) -> -- Has the launch button been pressed?
   (pongIsPaddleChanged : Bool) -> -- Has the direction of the paddle changed?
   (pongRemotePaddleUp : Bool) ->
   (pongRemotePaddleDown : Bool) ->
   (pongNetworkMode : PongNetworkMode) ->
   (pongRemoteAddr : SocketAddress) ->
   (pongRemotePort : Port) ->
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

initBall : Int -> Int -> PongBall
initBall lp_x lp_y = MkPongBall (ball_x, ball_y) 0 0 True True
  where ball_x = lp_x + PADDLE_WIDTH + (BALL_DIAMETER `div` 2)
        ball_y = lp_y + (PADDLE_HEIGHT `div` 2) 


getLocalPaddlePos : { [STATE GameState] } Eff IO (Int, Int)
getLocalPaddlePos = do
  st <- get
  if isServer st then 
    return $ pongLeftPaddlePos st
  else
    return $ pongRightPaddlePos st


updateRemotePaddlePos : { [STATE GameState] } Eff IO ()
updateRemotePaddlePos = do
  st <- get
  let lpp = pongLeftPaddlePos st
  let rpp = pongRightPaddlePos st
  let up = pongRemotePaddleUp st
  let down = pongRemotePaddleDown st
  let w = pongWidth st
  let h = pongHeight st
  if (isServer st) then do
    let new_pos = newPaddlePos rpp up down w h
    put $ record { pongRightPaddlePos = new_pos } st
  else do
    let new_pos = newPaddlePos lpp up down w h
    put $ record { pongLeftPaddlePos = new_pos } st

updateLocalPaddlePos : { [STATE GameState] } Eff IO Bool
updateLocalPaddlePos = do
  st <- get
  let lpp = pongLeftPaddlePos st
  let rpp = pongRightPaddlePos st
  let w = pongWidth st
  let h = pongHeight st
  let up = pongIsUpPressed st
  let down = pongIsDownPressed st
  let paddle_changed = pongIsPaddleChanged st
  if isServer st then do 
    let new_pos = newPaddlePos lpp up down w h
    put $ record { pongLeftPaddlePos = new_pos, 
                   pongIsPaddleChanged = False} st
    return paddle_changed
  else do
    let new_pos = newPaddlePos rpp up down w h
    put $ record { pongRightPaddlePos = new_pos, 
                   pongIsPaddleChanged = False } st
    return paddle_changed

  
drawPaddles : { [SDL_ON, STATE GameState] } Eff IO ()
drawPaddles = do st <- get
                 drawPaddle (pongLeftPaddlePos st)
                 drawPaddle (pongRightPaddlePos st)
  where drawPaddle : (Int, Int) -> { [SDL_ON] } Eff IO ()
        drawPaddle (x, y) = rectangle black x y PADDLE_WIDTH PADDLE_HEIGHT


drawBall : { [SDL_ON, STATE GameState] } Eff IO ()
drawBall = do
  st <- get
  let (MkPongBall (x, y) _ _ _ _) = pongBall st
  ellipse blue x y BALL_DIAMETER BALL_DIAMETER

initState : Width -> Height -> PongNetworkMode -> SocketAddress -> Port -> GameState 
initState w h = let (lp_x, lp_y) = initLPaddlePos w h in
  MkGameState w h (initBall lp_x lp_y)  (lp_x, lp_y) (initRPaddlePos w h) 
    False False False False False False

-- Update communication:
--  * Ball: The server does collision detection and so on, and only sends details of changes:
--          that is, if the direction of the ball changes due to a collision, or if it goes out of bounds.
-- 
--          The client updates the local ball position based on the latest information from the server.
--    (aside: it might also be prudent to periodically (ie, every 200ms or so) send a "back on track" msg
-- 
--  * Paddle: I think we can get away with sending an update every time the paddle moves. If not, that's 
--            fine: we can probably just send the deltas again.


launchBall : { [STATE GameState] } Eff IO ()
launchBall = do
  st <- get
  let ball' = record { pongBallXVel = LAUNCH_VELOCITY, pongBallStuck = False } (pongBall st)
  put $ record { pongBall = ball' } st

requiresLaunching : { [STATE GameState] } Eff IO Bool
requiresLaunching = do
  st <- get
  let ball = pongBall st
  -- In either case, reset the toLaunch flag
  put $ record { pongToLaunch = False } st
  return (pongToLaunch st && (pongBallStuck ball))

updateBallPos : { [STATE GameState] } Eff IO ()
updateBallPos = do
  st <- get
  let ball = pongBall st
  let (bp_x, bp_y) = pongBallPos ball
  let ball' =
    if (pongBallStuck ball) then 
      (uncurry initBall) (pongLeftPaddlePos st)
    else 
      record { pongBallPos = (bp_x + (pongBallXVel ball), 
                              bp_y + (pongBallYVel ball)) } ball
  put $ record { pongBall = ball' } st
  

inBox : PongBall -> (Int, Int) -> Width -> Height -> Bool
inBox (MkPongBall (pb_x, pb_y) _ _ _ _) (x, y) w h =
  pb_x > x && pb_x < (x + w) && pb_y > y && pb_y < (y + h)

hitPaddle : PongBall -> (Int, Int) -> Bool
hitPaddle pb pos = inBox pb pos PADDLE_WIDTH PADDLE_HEIGHT

-- Takes paddle position, ball position, returns movement velocities.
-- Funky angle logic from here: http://gamedev.stackexchange.com/questions/4253/how-do-you-calculate-where-a-ball-should-go-when-it-bounces-off-the-bar
paddleBounceVelocities : (Int, Int) -> (Int, Int) -> (Int, Int)
paddleBounceVelocities (p_x, p_y) (b_x, b_y) = unsafePerformIO ( do putStrLn $ "Rel intersect: " ++ (show rel_intersect)
                                                                    putStrLn $ "NI: " ++ (show normalised_intersect) 
                                                                    putStrLn $ "Bounce angle: " ++ (show bounce_angle)
                                                                    putStrLn $ "X, Y: " ++ (show (x_vel, y_vel))
                                                                    return (x_vel, y_vel))
  where rel_intersect : Float 
        rel_intersect = (cast p_y) + ((cast PADDLE_HEIGHT) / 2) - (cast b_y)
        normalised_intersect : Float
        normalised_intersect = rel_intersect / ((cast PADDLE_HEIGHT) / 2)
        bounce_angle : Float
        bounce_angle = normalised_intersect * MAX_ANGLE
        x_vel : Int
        x_vel = cast $ (cos bounce_angle) * (cast LAUNCH_VELOCITY)
        y_vel : Int
        y_vel = (-(cast $ (sin bounce_angle) * (cast LAUNCH_VELOCITY)))

-- Only check the opposite paddle to the one that was last hit.
checkPaddleCollisions : { [STATE GameState] } Eff IO Bool
checkPaddleCollisions = do 
  st <- get
  let ball = pongBall st
  let bp = pongBallPos ball
  let lpp = pongLeftPaddlePos st
  let rpp = pongRightPaddlePos st
  let left = pongBallHitLeft ball
  if (left && hitPaddle ball rpp) then do
    let (x_vel, y_vel) = paddleBounceVelocities rpp bp
    let ball' = record { pongBallXVel = -x_vel,
                         pongBallYVel = y_vel,
                         pongBallHitLeft = False } ball
    put $ record { pongBall = ball' } st
    return True
  else if ((not left) && hitPaddle ball lpp) then do
    let (x_vel, y_vel) = paddleBounceVelocities lpp bp
    let ball' = record { pongBallXVel = x_vel,
                         pongBallYVel = y_vel,
                         pongBallHitLeft = True } ball
    put $ record { pongBall = ball' } st
    return True
  else
    return False

-- Check the left and right walls
checkLRBounds : { [STATE GameState] } Eff IO Bool
checkLRBounds = do
  st <- get
  let (lp_x, lp_y) = pongLeftPaddlePos st
  let (MkPongBall (x, y) _ _ _ _) = pongBall st
  if (x < 0 || x > (pongWidth st)) then do
    put $ record { pongBall = initBall lp_x lp_y } st
    return True
  else
    return False

-- Check top and bottom walls
checkTBBounds : { [STATE GameState] } Eff IO Bool
checkTBBounds = do
  st <- get
  let ball = pongBall st
  let (x, y) = pongBallPos ball
  let yvel = pongBallYVel ball

  if (y < 0) then do
    let ball' = record { pongBallPos = (x, 0), pongBallYVel = yvel * -1 } ball
    put $ record { pongBall = ball' } st
    return True
  else if (y > pongHeight st) then do
    let ball' = record { pongBallPos = (x, pongHeight st), pongBallYVel = yvel * -1 } ball
    put $ record { pongBall = ball' } st
    return True
  else return False
 -- if (y < 0 || y > (pongHeight st)) then do
    
-- Updating ball position on the server peer: what we'll need to do here is firstly check if 
-- the ball needs "launching" -- that is, it's on a paddle, and space has been pressed.
-- If so, we launch it (launchBall). 
-- If not, we do collision checking: checking that the ball has hit the t&b walls --> reverse Y velocity
--                                   checking that the ball has hit the paddles --> crazy paddle stuff
--                                   checking that the ball has hit the R&L walls --> reset
updateBallServer : { [STATE GameState] } Eff IO Bool
updateBallServer = do
  requires_launching <- requiresLaunching
  when requires_launching launchBall
  -- TEMP: Just so we know the basics work...
  paddle_collision <- checkPaddleCollisions
  lr_bounds <- checkLRBounds
  tb_bounds <- checkTBBounds
  updateBallPos -- Apply X and Y velocities to move the ball
  return $ requires_launching ||  paddle_collision ||  lr_bounds || tb_bounds
