module GameMessage
import GameState
import IdrisNet.PacketLang
import IdrisNet.UDP.UDPClient
import Effect.StdIO

-- Data structure communicated between aux thread and master thread,
-- signifying some update.
data GameMessage = UpdateRemotePaddle Bool Bool Int Int -- Up pressed, down pressed, x, y
                 | UpdateRemoteBallPos PongBall



ballUpdate : PacketLang
ballUpdate = with PacketLang do
  x <- bits 32
  y <- bits 32
  xv_neg <- bool
  x_vel <- bits 32
  yv_neg <- bool
  y_vel <- bits 32
  bool -- Stuck to paddle?
  bool -- Last hit left

paddleUpdate : PacketLang
paddleUpdate = with PacketLang do
  bool
  bool
  bits 32 -- X
  bits 32 -- Y

statusUpdate : PacketLang
statusUpdate = with PacketLang do
  is_ball <- bool -- True if ball update, false if paddle
  p_if is_ball then ballUpdate else paddleUpdate
 
mkMessage : (mkTy statusUpdate) -> GameMessage
mkMessage (True ## x ## y ## xv_neg ## xv ## yv_neg ## yv ## stuck ## left) = 
    UpdateRemoteBallPos $ MkPongBall (val x, val y) xv' yv' stuck left
  where xv' = if xv_neg then ((val xv) * (-1)) else (val xv)
        yv' = if yv_neg then ((val yv) * (-1)) else (val yv)

mkMessage (False ## up ## down ## x ## y) = UpdateRemotePaddle up down (val x) (val y) 

sendBallUpdate : { [STATE GameState, STDIO, UDPCLIENT] } Eff IO Bool
sendBallUpdate = do
    st <- get
    let sa = pongRemoteAddr st
    let p = pongRemotePort st
    case m_pckt (pongBall st) of
      Just pckt => do IdrisNet.UDP.UDPClient.udpWritePacket sa p statusUpdate pckt
                      return True
      Nothing => return False
  where m_pckt : PongBall -> (Maybe (mkTy statusUpdate))
        m_pckt pb = with Monad do 
                       let ((x, y), xv, yv) = 
                         (pongBallPos pb, pongBallXVel pb, pongBallYVel pb)
                       b_x <- mkBounded 32 x
                       b_y <- mkBounded 32 y
                       let neg_xv = xv < 0
                       b_xv <- mkBounded 32 (abs xv)
                       let neg_yv = yv < 0
                       b_yv <- mkBounded 32 (abs yv)
                       return (True ## b_x ## b_y ## neg_xv ## b_xv ## 
                               neg_yv ## b_yv ## (pongBallStuck pb) ## (pongBallHitLeft pb))

sendPaddleUpdate : { [STATE GameState, UDPCLIENT] } Eff IO Bool
sendPaddleUpdate = do
    st <- get
    let sa = pongRemoteAddr st
    let p = pongRemotePort st
    let up_pressed = pongIsUpPressed st
    let down_pressed = pongIsDownPressed st
    case m_pckt up_pressed down_pressed !getLocalPaddlePos of
      Just pckt => do IdrisNet.UDP.UDPClient.udpWritePacket sa p statusUpdate pckt
                      return True
      Nothing => return False
  where m_pckt : Bool -> Bool -> (Int, Int) -> (Maybe (mkTy statusUpdate))
        m_pckt up_pressed down_pressed (x, y) = with Monad do 
          b_x <- mkBounded 32 x
          b_y <- mkBounded 32 y
          return (False ## up_pressed ## down_pressed ## b_x ## b_y)

