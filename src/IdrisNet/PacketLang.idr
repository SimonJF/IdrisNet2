module IdrisNet2.PacketLang 
import Language.Reflection
import Data.So
import Data.Vect

%access public

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

intToNat : Int -> Nat
intToNat 0 = Z
intToNat i = if i < 0 then Z else (assert_total (S (intToNat (i - 1))))

strLen : String -> Int
strLen s = natToInt $ length s

data Bounded : Nat -> Type where
  BInt : (x : Int) -> (prf : So (x < (pow 2 i))) -> Bounded i

instance Show (Bounded i) where
  show (BInt x _) = show x

val : Bounded i -> Int
val (BInt i p) = i

mkBounded : (bound : Nat) -> Int -> Maybe (Bounded bound)
mkBounded b i_n = 
  case choose (i_n < (pow 2 b)) of
      Left yes => Just (BInt i_n yes)
      Right _ => Nothing 

NonZero : Nat -> Type
NonZero n = n `GT` 0

-- Primitive Binary Chunks
data Chunk : Type where
  -- Bits must be at least 1 wide
  Bit : (width : Nat) -> NonZero width -> Chunk
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
  Decodable : (n : Nat) -> 
              (t : Type) -> 
              (Bounded n -> Maybe t) -> 
              (t -> Bounded n) -> Chunk

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
  propTy (P_BOOL b) = So b
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
chunkTy (Decodable n t encode_fn decode_fn) = t

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
bitLength : (pl : PacketLang) -> mkTy pl -> Length
chunkLength : (c : Chunk) -> chunkTy c -> Length
chunkLength (Bit w p) _ = natToInt w
chunkLength CBool _ = 1
chunkLength CString str = 8 * ((strLen str) + 1) 
chunkLength (LString len) str = 8 * (natToInt len)
chunkLength (Prop _) p = 0 -- Not written to the packet
chunkLength (Decodable n _ _ _) _ = natToInt n

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
bit : (w : Nat) -> {auto p : NonZero w} -> Chunk
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
syntax decodable [n] [ty] [fn1] [fn2] = (CHUNK (Decodable n ty fn1 fn2))
-- Propositions
syntax prop_bool [p] = (P_BOOL p)
syntax prop_or [p1] [p2] = (P_OR p1 p2)
syntax prop_and [p1] [p2] = (P_AND p1 p2)
syntax prop_eq [p1] [p2] = (P_EQ p1 p2)

-- Additional sugar
syntax [x] "##" [y] = (x ** y)


