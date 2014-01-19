module PacketStruct
import Network.PacketLang

%access public

simpleStruct : PacketLang
simpleStruct = do
  cstring
  lstring 5
  p_either (bits 8) (lstring 4)
  listn 3 cstring
  bool
  bool

myBoundedInt : Bounded 8
myBoundedInt = BInt 5 oh

simpleStructInstance : (mkTy simpleStruct)
simpleStructInstance = ("hello" ##
                        "world" ##
                        (Left myBoundedInt) ## 
                        ["hello", "you", "dears"] ##
                        True ##
                        False)

simpleResponse : PacketLang
simpleResponse = do
  cstring
  cstring

simpleResponseInstance : (mkTy simpleResponse)
simpleResponseInstance = "Got" ## "It!"
