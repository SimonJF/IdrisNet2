module PacketStruct
import IdrisNet.PacketLang
import Data.So

%access public

simpleStruct : PacketLang
simpleStruct = do
  cstring
  lstring 5
  p_either (bits 32) (lstring 4)
  listn 3 cstring
  b1 <- bool
  b2 <- bool
  prop (prop_or (prop_bool b2) 
                (prop_bool b1)) 

myBoundedInt : Bounded 32
myBoundedInt = BInt 9001 Oh

simpleStructInstance : (mkTy simpleStruct)
simpleStructInstance = ("hello" ##
                        "world" ##
                        (Left myBoundedInt) ## 
                        ["hello", "you", "dears"] ##
                        True ##
                        False ##
                        (Right Oh))

simpleResponse : PacketLang
simpleResponse = do
  cstring
  cstring

simpleResponseInstance : (mkTy simpleResponse)
simpleResponseInstance = "Got" ## "It!"

