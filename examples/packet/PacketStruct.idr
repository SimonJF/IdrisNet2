module PacketStruct
import Network.PacketLang

%access public

simpleStruct : PacketLang
simpleStruct = do
  cstring
  lstring 5
  p_either (bits 32) (lstring 4)
  listn 3 cstring
  b1 <- bool
  b2 <- bool
  prop_or (P_BOOL b2) (P_BOOL b1) -- FIXME
  -- CHUNK (Prop (P_OR (P_BOOL b2) (P_BOOL b1)))
  -- CHUNK (Prop (P_OR (P_BOOL b2) (P_BOOL b1)))

myBoundedInt : Bounded 32
myBoundedInt = BInt 9001 oh

simpleStructInstance : (mkTy simpleStruct)
simpleStructInstance = ("hello" ##
                        "world" ##
                        (Left myBoundedInt) ## 
                        ["hello", "you", "dears"] ##
                        True ##
                        False ##
                        (Right oh))

simpleResponse : PacketLang
simpleResponse = do
  cstring
  cstring

simpleResponseInstance : (mkTy simpleResponse)
simpleResponseInstance = "Got" ## "It!"
