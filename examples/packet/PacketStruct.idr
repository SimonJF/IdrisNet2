module PacketStruct
import Network.PacketLang

%access public

simpleStruct : PacketLang
simpleStruct = do
  cstring
  lstring 5
  p_either (bits 8) (lstring 4)
  listn 3 cstring
  b1 <- bool
  b2 <- bool
  prop (prop_or (prop_bool b2) (prop_bool b1))
  -- CHUNK (Prop (P_OR (P_BOOL b2) (P_BOOL b1)))
  -- CHUNK (Prop (P_OR (P_BOOL b2) (P_BOOL b1)))

myBoundedInt : Bounded 8
myBoundedInt = BInt 5 oh

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
