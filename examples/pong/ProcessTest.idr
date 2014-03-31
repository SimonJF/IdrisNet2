module Main

import Effects
import Process
import Effect.StdIO


myProcessOne : RunningProcess String IO [STDIO]
myProcessOne = do 
  msg <- recvMessage
  putStr $ "Consumer process received message: " ++ msg ++ "\n"
  myProcessOne

myProcessTwo : ProcPID String -> Nat -> { [PROCESS (Running String)] } Eff IO ()
myProcessTwo _ Z = return ()
myProcessTwo pid (S k) = do
  sendMessage pid (show k)
  myProcessTwo pid k

setupProcesses : { [PROCESS (Running String), STDIO] } Eff IO ()
setupProcesses = do
  pid_one <- spawn String myProcessOne [()]
  myProcessTwo pid_one 10
  return ()

main : IO ()
main = do runInit [initProcess, ()] setupProcesses
          getLine
          return ()

