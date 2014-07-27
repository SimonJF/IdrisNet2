{- Process-based concurrency effect. -}
module Effect.Process
import System.Concurrency.Raw
import Effects
import Effect.State
import Effect.StdIO

%access public

-- Type of a channel, parameterised over message type. Ptr is the VM Ptr
data ProcPID msg = MkPPid Ptr


mutual
  data Running : Type -> Type where
    MkProc : Running mty

  {- Type of a running process, including message type, execution context,
     input and output effects. -}
  RunningProcessM : (mTy : Type) -> 
                   List EFFECT -> 
                   List EFFECT -> 
                   Type
  RunningProcessM mty effs effs' = 
    Eff () ((PROCESS (Running mty)) :: effs) 
             (\_ => (PROCESS (Running mty)) :: effs')

  {- RunningProcessM but with the same input and output effects -}
  RunningProcess : (mTy : Type) ->
                   List EFFECT ->
                   Type
  RunningProcess mty effs = RunningProcessM mty effs effs


  data Process : Effect where
    Spawn : (mty : Type) ->
            RunningProcessM mty effs effs' -> 
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

-- Spawns a new thread using the given message type, thread, and environment
spawn : (mty : Type) -> 
        RunningProcessM mty effs effs' -> 
        Env IO effs ->
        { [PROCESS (Running mty')] } Eff (ProcPID mty)
spawn ty proc env = call $ Spawn ty proc env

-- Checks whether a message has been received
hasMessage : { [PROCESS (Running mty)] } Eff Bool
hasMessage = call HasMessageWaiting

-- Sends a message
sendMessage : ProcPID mty -> mty -> { [PROCESS (Running mty)] } Eff ()
sendMessage pid msg = call $ SendMessage pid msg

-- Receives a message
recvMessage : { [PROCESS (Running mty)] } Eff mty
recvMessage = call RecvMessage

-- Receives a message, returning the PID of the sending process
recvMessageAddr : { [PROCESS (Running mty)] } Eff (ProcPID mty, mty)
recvMessageAddr = call RecvMessageAddr

-- Returns the local PID
getPID : { [PROCESS (Running mty)] } Eff (ProcPID mty) 
getPID = call GetID

-- Type synonym for constructing instances
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
      
