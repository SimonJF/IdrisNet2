module Process
import System.Concurrency.Raw
import Effects
import Effect.State
import Effect.StdIO

%access public

data ProcPID msg = MkPPid Ptr

-- FIXME: Had to specialise handlers to IO. Probably a way to make this nicer.

mutual
  data Running : Type -> Type where
    MkProc : Running mty

  RunningProcessM : (mTy : Type) -> 
                   (m : Type -> Type) -> 
                   List EFFECT -> 
                   List EFFECT -> 
                   Type
  RunningProcessM mty m effs effs' = 
    Eff m () ((PROCESS (Running mty)) :: effs) 
             (\_ => (PROCESS (Running mty)) :: effs')

  RunningProcess : (mTy : Type) ->
                   (m : Type -> Type) ->
                   List EFFECT ->
                   Type
  RunningProcess mty m effs = RunningProcessM mty m effs effs


  data Process : Effect where
    Spawn : (mty : Type) ->
            RunningProcessM mty IO effs effs' -> 
            Env IO effs -> 
            { (Running mty') } Process (ProcPID mty)

    -- Returns true if there's a message waiting in this process' mailbox, 
    -- false if not
    HasMessageWaiting : { (Running mty) } Process Bool

    -- Sends a message to a given process
    SendMessage : ProcPID mty -> mty -> { (Running mty) } Process ()

    -- Receives a message from a given process
    RecvMessage : { (Running mty) } Process mty
    
    RecvMessageAddr : { (Running mty) } Process (ProcPID mty, mty)

    -- Gets local PID
    GetID : { (Running mty) } Process (ProcPID mty)

   PROCESS : Type -> EFFECT
   PROCESS t = MkEff t Process


spawn : (mty : Type) -> 
        RunningProcessM mty IO effs effs' -> 
        Env IO effs ->
        { [PROCESS (Running mty')] } Eff IO (ProcPID mty)
spawn ty proc env = Spawn ty proc env

hasMessage : { [PROCESS (Running mty)] } Eff IO Bool
hasMessage = HasMessageWaiting

sendMessage : ProcPID mty -> mty -> { [PROCESS (Running mty)] } Eff IO ()
sendMessage pid msg = SendMessage pid msg

recvMessage : { [PROCESS (Running mty)] } Eff IO mty
recvMessage = RecvMessage

recvMessageAddr : { [PROCESS (Running mty)] } Eff IO (ProcPID mty, mty)
recvMessageAddr = RecvMessageAddr

getPID : { [PROCESS (Running mty)] } Eff IO (ProcPID mty) 
getPID = GetID

initProcess : Running mty
initProcess = MkProc

private
getInternal : Running mty -> IO (Ptr, mty)
getInternal x = getMsg


instance Handler Process IO where
  handle MkProc (Spawn ty proc env) k = do
    ptr <- fork (runInit (MkProc :: env) proc)
    k (MkPPid ptr) MkProc

  handle MkProc GetID k = 
    k (MkPPid prim__vm) MkProc

  handle MkProc HasMessageWaiting k = do
    res <- checkMsgs
    k res MkProc

  handle MkProg (SendMessage (MkPPid pid) msg) k = do
    sendToThread pid (prim__vm, msg)
    k () MkProc

  handle x RecvMessage k = do
    (_, msg) <- getInternal x
    k msg x
  
  handle x RecvMessageAddr k = do
    (pid, msg) <- getInternal x
    k (MkPPid pid, msg) x
      
