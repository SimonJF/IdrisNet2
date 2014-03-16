module Network.PacketLang 
import Language.Reflection

%access public

-- Start off with a direct translation of the data types from the 
-- paper and go from there

-- Propositions about data
data Proposition : Type where
  P_LT : Nat -> Nat -> Proposition
  P_EQ : DecEq a => a -> a -> Proposition
  P_BOOL : Bool -> Proposition
  P_AND : Proposition -> Proposition -> Proposition
  P_OR : Proposition -> Proposition -> Proposition

Length : Type
Length = Int

-- grrrrr, hackity hack
natToInt : Nat -> Int
natToInt Z = 0
natToInt (S k) = 1 + (natToInt k)

-- FIXME: This is a horrible, horrible, horrible, horrible falsehood.
-- But for now, it'll do.
%assert_total
intToNat : Int -> Nat
intToNat 0 = Z
intToNat i = S (intToNat (i - 1))

strLen : String -> Int
strLen s = natToInt $ length s

data Bounded : Nat -> Type where
  BInt : (x : Int) -> (prf : so (x < (pow 2 i))) -> Bounded i

instance Show (Bounded i) where
  show (BInt x _) = show x

val : Bounded i -> Int
val (BInt i p) = i



-- Primitive Binary Chunks
data Chunk : Type where
  -- Bits must be at least 1 wide
  Bit : (width : Nat) -> so (width > 0) -> Chunk
  -- Boolean value, stored as one bit.
  -- Convenience, so we can marshal / unmarshal directly as a Bool
  CBool : Chunk
  -- Native C String, null terminated
  CString : Chunk
  -- String with fixed bounded length
  LString : Nat -> Chunk
  -- String with dynamic bounded length
  --LString : ((length s) ** (s : String)) -> Chunk
  -- Proposition about data
  Prop : (P : Proposition) -> Chunk
  -- Custom chunk of binary data.
  -- Can be used to make cleverer things without having
  -- to add to the core PL each time.

infixl 5 //
--infixl 5 ##

mutual
  -- Requires two propositions, and evidence that they're true.
  -- For example, if we have P_AND P_BOOL P_BOOL, we'd need two 'oh' proofs.
  -- If we had two P_EQ propositions, we'd need two reflexivity proofs.
  data Both : Proposition -> Proposition -> Type where
    MkBoth : (propTy a) -> (propTy b) -> Both a b

  -- Decode propositions into Idris types.
  propTy : Proposition -> Type
  propTy (P_LT x y) = LT x y
  propTy (P_EQ x y) = x=y
  propTy (P_BOOL b) = so b
  propTy (P_AND s t) = Both s t
  propTy (P_OR s t) = Either (propTy s) (propTy t)

-- Decode chunks into Idris types
-- TODO <<
chunkTy : Chunk -> Type
chunkTy (Bit w p) = Bounded w 
chunkTy CString = String
chunkTy (LString i) = String
chunkTy (Prop p) = propTy p
chunkTy (CBool) = Bool

-- Packet Language
mutual
  data PacketLang : Type where
    CHUNK : (c : Chunk) -> PacketLang
    IF : (test : Bool) -> (yes : PacketLang) -> (no : PacketLang) -> PacketLang
    -- // : or
    (//) : PacketLang -> PacketLang -> PacketLang
    LIST : PacketLang -> PacketLang
    LISTN : (n : Nat) -> PacketLang -> PacketLang
    NULL : PacketLang -- Sometimes, we want to signify that there's nothing there
    (>>=) : (p : PacketLang) -> (mkTy p -> PacketLang) -> PacketLang
    
  -- Packet language decoding
  mkTy : PacketLang -> Type
  mkTy (CHUNK c) = chunkTy c
  mkTy (IF x t e) = if x then (mkTy t) else (mkTy e)
  mkTy (l // r) = Either (mkTy l) (mkTy r)
  mkTy (LIST x) = List (mkTy x)
  mkTy (LISTN n a) = Vect n (mkTy a)
  mkTy NULL = ()
  mkTy (c >>= k) = (x ** mkTy (k x))


{- Chunk length in bits -}
total
bitLength : (pl : PacketLang) -> mkTy pl -> Length
chunkLength : (c : Chunk) -> chunkTy c -> Length
chunkLength (Bit w p) _ = natToInt w
chunkLength CBool _ = 1
-- TODO: This doesn't take into account if there's a null character
-- within the string itself. I had something nice using span earlier,
-- but it didn't work (probably due to a library bug)
chunkLength CString str = 8 * ((strLen str) + 1) 
chunkLength (LString len) str = 8 * (natToInt len)
chunkLength (Prop _) p = 0 -- Not written to the packet

listLength : (pl : PacketLang) -> List (mkTy pl) -> Length
listLength pl [] = 0
listLength pl (x :: xs) = bitLength pl x + (listLength pl xs)

vectLength : (pl : PacketLang) -> Vect n (mkTy pl) -> Length
vectLength pl [] = 0
vectLength pl (x :: xs) = bitLength pl x + (vectLength pl xs)

bitLength (CHUNK c) x = chunkLength c x
bitLength (IF True yes _) x = bitLength yes x
bitLength (IF False _ no) x = bitLength no x
bitLength (y // z) x = either (\l_x => bitLength y l_x) (\r_x => bitLength z r_x) x
bitLength (LIST pl) x = listLength pl x
bitLength (LISTN n pl) x = vectLength pl x
bitLength NULL _ = 0
bitLength (c >>= k) (a ** b) = bitLength c a + bitLength (k a) b


-- Syntax rules, so it's nicer to write these things...
bit : (w : Nat) -> {default proof { refine oh;} 
                     p : so (w > 0) } 
                -> Chunk
bit w {p} = Bit w p

-- syntax bit [x] = Bit x oh
--syntax bytes [n] = CHUNK (bit (n * 8))
--syntax bounded [x] = BInt x oh

-- PacketLang DSL syntax
syntax bits [n] = (CHUNK (bit n))
syntax check [b] = (CHUNK (Prop (P_BOOL b)))
syntax lstring [n] = (CHUNK (LString n))
syntax cstring = (CHUNK (CString))
syntax listn [n] [t] = (LISTN n t)
syntax list [t] = (LIST t)
syntax p_if [b1] then [b2] else [b3] = (IF b1 b2 b3)
syntax p_either [t1] [t2] = (t1 // t2)
syntax bool = (CHUNK (CBool))
syntax prop [p] = (CHUNK (Prop p))
syntax null = NULL

-- Propositions
syntax prop_bool [p] = (P_BOOL p)
syntax prop_or [p1] [p2] = (P_OR p1 p2)
syntax prop_and [p1] [p2] = (P_AND p1 p2)
syntax prop_eq [p1] [p2] = (P_EQ p1 p2)

-- Additional sugar
syntax [x] "##" [y] = (x ** y)


