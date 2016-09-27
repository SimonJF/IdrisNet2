module IdrisNet.Packet
import Language.Reflection
import IdrisNet.PacketLang
import Network.Socket
import Effects
import Data.So
import Data.Vect


%access public export
%include C "bindata.h"
%link C "bindata.o"

-- Type synonyms for different arguments to foreign functions
public export
BytePos : Type
BytePos = Int

public export
Position : Type
Position = Int

public export
ByteData : Type
ByteData = Int

public export
data ActivePacket : Type where
  ActivePacketRes : BufPtr -> BytePos -> Length -> ActivePacket

public export
dumpPacket : BufPtr -> Length -> IO ()
dumpPacket (BPtr pckt) len =
  foreign FFI_C "dumpPacket" (Ptr -> Int -> IO Unit) pckt len


{- Internal FFI Functions -}
foreignCreatePacket : Int -> IO BufPtr
foreignCreatePacket len = map BPtr $ foreign FFI_C "newPacket" (Int -> IO Ptr) len

foreignSetByte : BufPtr -> Position -> ByteData -> IO ()
foreignSetByte (BPtr pckt) dat pos = 
  foreign FFI_C "setPacketByte" (Ptr -> Int -> Int -> IO Unit) pckt dat pos

foreignSetBits : BufPtr -> Position -> Position -> ByteData -> IO ()
foreignSetBits (BPtr pckt) start end dat = 
  foreign FFI_C "setPacketBits" (Ptr -> Int -> Int -> Int -> IO Unit) pckt start end dat

foreignSetString : BufPtr -> Position -> String -> Int -> Char -> IO ()
foreignSetString (BPtr pckt) start dat len term =
  foreign FFI_C "setPacketString" (Ptr -> Int -> String -> Int -> Char -> IO Unit) pckt start dat len term

foreignGetByte : BufPtr -> Position -> IO ByteData
foreignGetByte (BPtr pckt) pos = 
  foreign FFI_C "getPacketByte" (Ptr -> Int -> IO Int) pckt pos

foreignGetBits : BufPtr -> Position -> Position -> IO ByteData
foreignGetBits (BPtr pckt) start end =
  foreign FFI_C "getPacketBits" (Ptr -> Int -> Int -> IO Int) pckt start end

{- Marshalling -}

marshalChunk : ActivePacket -> (c : Chunk) -> (chunkTy c) -> IO Length
marshalChunk (ActivePacketRes pckt pos p_len) (Bit w p) (BInt dat p2) = do
  let len = chunkLength (Bit w p) (BInt dat p2)
  foreignSetBits pckt pos (pos + (natToInt w) - 1) dat
  return len
marshalChunk (ActivePacketRes pckt pos p_len) CBool b = do
  let bit = the Int $ if b then 1 else 0
  foreignSetBits pckt pos pos bit
  return (chunkLength CBool b)
marshalChunk (ActivePacketRes pckt pos p_len) CString str = do
  let len = chunkLength CString str
  --putStrLn $ "CStr length: " ++ (show len)
  foreignSetString pckt pos str len '\0'
  return len
marshalChunk (ActivePacketRes pckt pos p_len) (LString n) str = do
  let len = chunkLength (LString n) str 
  foreignSetString pckt pos str len '\0'
  return len
marshalChunk (ActivePacketRes pckt pos p_len) (Decodable n t _ encode_fn) to_encode = do
  let code = encode_fn to_encode
  let len = natToInt n
  foreignSetBits pckt pos (pos + len - 1) (val code)
  return len

marshalChunk (ActivePacketRes pckt pos p_len) (Prop _) x2 = return 0 -- We're not doing anything
  
marshalList : ActivePacket -> (pl : PacketLang) -> List (mkTy pl) -> IO Length
marshalVect : ActivePacket -> (pl : PacketLang) -> Vect n (mkTy pl) -> IO Length

marshal' : ActivePacket -> (pl : PacketLang) -> mkTy pl -> IO Length
marshal' ap (CHUNK c) c_dat = marshalChunk ap c c_dat
marshal' ap (IF True pl_t _) ite = marshal' ap pl_t ite
marshal' ap (IF False _ pl_f) ite = marshal' ap pl_f ite
marshal' ap (pl_1 // pl_2) x = either (\x_l => marshal' ap pl_1 x_l)
                                      (\x_r => marshal' ap pl_2 x_r) x
marshal' ap (LIST pl) xs = marshalList ap pl xs
marshal' ap (LISTN n pl) xs = marshalVect ap pl xs
marshal' _ NULL _ = return 0
marshal' ap (c >>= k) (x ** y) = do
  len <- marshal' ap c x
  let (ActivePacketRes pckt pos p_len) = ap
  let ap2 = (ActivePacketRes pckt (pos + len) p_len) 
  len2 <- marshal' ap2 (k x) y
  return $ len + len2


{- Marshal PacketLang to ByteData -}
--marshalVect : ActivePacket -> (pl : PacketLang) -> Vect n (mkTy pl) -> IO Length
marshalVect ap pl [] = return 0
marshalVect (ActivePacketRes pckt pos p_len) pl (x::xs) = do
  len <- marshal' (ActivePacketRes pckt pos p_len) pl x
  xs_len <- marshalVect (ActivePacketRes pckt (pos + len) p_len) pl xs
  return $ len + xs_len

--marshalList : ActivePacket -> (pl : PacketLang) -> List (mkTy pl) -> IO Length
marshalList ap pl [] = return 0
marshalList (ActivePacketRes pckt pos p_len) pl (x::xs) = do
  len <- marshal' (ActivePacketRes pckt pos p_len) pl x
  xs_len <- marshalList (ActivePacketRes pckt (pos + len) p_len) pl xs
  return $ len + xs_len


{- Unmarshalling Code -}
public export
unmarshal' : ActivePacket -> (pl : PacketLang) -> Maybe (mkTy pl, Length)


--unmarshal' : ActivePacket -> (pl : PacketLang) -> Maybe (mkTy pl, Length)
unmarshalCString' : ActivePacket ->  
                    Int -> 
                    IO (Maybe (List Char, Length))
unmarshalCString' (ActivePacketRes pckt pos p_len) i with (pos + 8 <= p_len)
  -- Firstly we need to check whether we're within the bounds of the packet.
  -- If not, then the parse has failed. 
  -- If we're within bounds, we need to read the next character, and recursively
  -- call.
  | True = do
    next_byte <- foreignGetBits pckt pos (pos + 7)
    --putStrLn $ "Byte read: " ++ (show next_byte)
    let char = chr next_byte
    --putStrLn $ "Char: " ++ (show char)
    -- If we're up to a NULL, we've read the string
    if (char == '\0') then do
      return $ Just ([], 8)
    else do -- Otherwise, recursively call
      -- We're assuming sizeof(char) = 8 here
      rest <- unmarshalCString' (ActivePacketRes pckt (pos + 8) p_len) (i + 8)
      case rest of Just (xs, j) => return $ Just (char::xs, j + 8)
                   Nothing => return Nothing
  | False = -- do --putStrLn $ "Pos: " ++ (show pos)
               --putStrLn $ "p_len: " ++ (show p_len) 
               return Nothing

unmarshalCString : ActivePacket -> IO (Maybe (String, Length))
unmarshalCString (ActivePacketRes pckt pos p_len) = do
  res <- unmarshalCString' (ActivePacketRes pckt pos p_len) 0
  case res of 
       Just (chrs, len) => return $ Just (pack chrs, len) 
       Nothing => return Nothing

unmarshalLString' : ActivePacket -> Nat -> IO (List Char)
unmarshalLString' ap Z = return []
unmarshalLString' (ActivePacketRes pckt pos p_len) (S k) = do
  next_byte <- foreignGetBits pckt pos (pos + 7)
  let char = chr next_byte
  rest <- unmarshalLString' (ActivePacketRes pckt (pos + 8) p_len) k
  return $ (char :: rest)

-- We've already bounds-checked the LString against the packet length,
-- meaning it's safe to just return a string.
unmarshalLString : ActivePacket -> Nat -> IO String
unmarshalLString ap n = map pack (unmarshalLString' ap n)


unmarshalBits : ActivePacket -> (c : Chunk) -> IO (Maybe (chunkTy c, Length))
unmarshalBits (ActivePacketRes pckt pos p_len) (Bit width p) with ((pos + (natToInt width) - 1) <= p_len)
  | True = do
    res <- foreignGetBits pckt pos (pos + (natToInt width) - 1)
    --putStrLn $ "Read: " ++ show res
    return $ Just $ (BInt res (assert_total $ believe_me Oh), (natToInt width)) -- Have to trust it, as it's from C
  | False = return Nothing
unmarshalBits _ _ = return Nothing


unmarshalBool : ActivePacket -> IO (Maybe (Bool, Length))
unmarshalBool (ActivePacketRes pckt pos p_len) with (pos <= p_len)
  | True = do
      res <- foreignGetBits pckt pos pos
      --putStrLn $ "Read: " ++ show (res == 1)
      return $ Just (res == 1, 1)
  | False = return Nothing

unmarshalProp : (p : Proposition) -> Maybe (propTy p)
unmarshalProp (P_EQ x y) = 
  case decEq x y of
    (Yes p) => Just p
    (No p) =>  Nothing
unmarshalProp (P_BOOL b) =
  case choose b of
    Left p_yes => Just p_yes
    Right _ => Nothing
-- Nothing -- That's just SO not b
unmarshalProp (P_AND prop1 prop2) = do
  p1 <- unmarshalProp prop1
  p2 <- unmarshalProp prop2
  Just (MkBoth p1 p2)
unmarshalProp (P_OR p1 p2) =
  maybe (maybe Nothing 
                    (\p2' => Just (Right p2')) (unmarshalProp p2))
                    (\p1' => Just (Left p1')) (unmarshalProp p1)
unmarshalProp (P_LT x y) = Nothing -- TODO


unmarshalChunk : ActivePacket -> (c : Chunk) -> IO (Maybe (chunkTy c, Length))
unmarshalChunk ap (Bit width p) = unmarshalBits ap (Bit width p) 
unmarshalChunk ap CBool = unmarshalBool ap 
unmarshalChunk ap CString = unmarshalCString ap
unmarshalChunk (ActivePacketRes pckt pos p_len) (LString n) =
  -- Do bounds checking now, if it passes then we're golden later on
  if pos + (8 * (natToInt n)) <= p_len then do
    res <- unmarshalLString (ActivePacketRes pckt pos p_len) n
    return $ Just (res, (8 * (natToInt n)))
  else do
 --   putStrLn "Check failed in unmarshal LString"
    return Nothing
unmarshalChunk (ActivePacketRes pckt pos p_len) (Decodable n t decode_fn _) = do
  let len = natToInt n
  let end_pos = (pos + len)
  if (end_pos <= p_len) then do
    dat <- foreignGetBits pckt pos (end_pos - 1) 
    case (decode_fn (BInt dat (believe_me Oh))) of -- Decode using supplied fn
      Just decoded_val => return $ Just (decoded_val, len)
      Nothing => return Nothing
  else return Nothing -- Out of bounds
unmarshalChunk _ (Prop p) = 
  case unmarshalProp p of
       Just p' => return $ Just (p', 0)
       Nothing => return Nothing
 

-- There's an ambiguity problem here, but it's not massively problematic
-- according to Edwin (in reality, RFCs will define markers at the end of
-- lists). 
unmarshalList : ActivePacket -> (pl : PacketLang) -> (List (mkTy pl), Length)
unmarshalList (ActivePacketRes pckt pos p_len) pl =
    case (unmarshal' (ActivePacketRes pckt pos p_len) pl) of
      Just (item, len) => 
        let (rest, rest_len) = unmarshalList (ActivePacketRes pckt (pos + len) p_len) pl in
--        let (rest, rest_len) = (fst xs_tup, snd xs_tup) in
          (item :: rest, len + rest_len)
      Nothing => ([], 0) -- Finished parsing list



unmarshalVect : ActivePacket -> 
                (pl : PacketLang) -> 
                (len : Nat) -> 
                Maybe ((Vect len (mkTy pl)), Length)
unmarshalVect _ _ Z = Just ([], 0)
unmarshalVect (ActivePacketRes pckt pos p_len) pl (S k) = do
  (item, len) <- unmarshal' (ActivePacketRes pckt pos p_len) pl 
  (rest, rest_len) <- unmarshalVect (ActivePacketRes pckt (pos + len) p_len) pl k
  return (item :: rest, len + rest_len)

-- unmarshal' : ActivePacket -> (pl : PacketLang) -> Maybe (mkTy pl, Length)
unmarshal' ap (CHUNK c) = unsafePerformIO $ unmarshalChunk ap c
unmarshal' ap (IF False yes no) = unmarshal' ap no
unmarshal' ap (IF True yes no) = unmarshal' ap yes
-- Attempt x, if correct then return x.
-- If not, try y. If correct, return y. 
-- If neither correct, return Nothing.
unmarshal' ap (x // y) = do
  let x_res = unmarshal' ap x
  --let y_res = 
  case x_res of
       Just (pckt, len) => Just (Left pckt, len)
       Nothing => case (unmarshal' ap y) of
                    Just (pckt, len) => Just (Right pckt, len)
                    Nothing => Nothing
unmarshal' ap (LIST pl) = Just (unmarshalList ap pl)
unmarshal' ap (LISTN n pl) = unmarshalVect ap pl n
unmarshal' ap NULL = Just ((), 0)
unmarshal' (ActivePacketRes pckt pos p_len) (c >>= k) = do
  (res, res_len) <- unmarshal' (ActivePacketRes pckt pos p_len) c 
  (res2, res2_len) <- unmarshal' (ActivePacketRes pckt (pos + res_len) p_len) (k res) 
  return ((res ** res2), res_len + res2_len)


{- Publicly-Facing Functions... -}
-- | Given a packet language and a packet, creates a BufPtr that
-- | may be sent over the network by a UDP or TCP socket
public export
marshal : (pl : PacketLang) -> (mkTy pl) -> IO (BufPtr, Length)
marshal pl dat = do
  let pckt_len = bitLength pl dat
  pckt <- foreignCreatePacket pckt_len
  len <- marshal' (ActivePacketRes pckt 0 pckt_len) pl dat
  -- putStrLn "Marshalling: "
--  dumpPacket pckt 1024
  let nat_len' = (intToNat len) `div` 8
  let len' = natToInt nat_len'
  return (pckt, len' + 1)

-- | Given a packet language and a BufPtr, unmarshals the packet
public export
unmarshal : (pl : PacketLang) -> 
            BufPtr -> 
            Length -> 
            IO (Maybe (mkTy pl, ByteLength))
unmarshal pl pckt len = do
  --putStrLn "Unmarshalling: "
--  dumpPacket pckt 1024
  return $ (unmarshal' (ActivePacketRes pckt 0 (len * 8)) pl) 
  

-- | Destroys a BufPtr
public export
freePacket : BufPtr -> IO ()
freePacket (BPtr pckt) = foreign FFI_C "freePacket" (Ptr -> IO Unit) pckt


