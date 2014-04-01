module DNSParser
import Network.Packet
import Network.PacketLang
import DNS
import Effects
import Effect.State

DNSReference : Type
DNSReference = Int

public
data DNSParseError = NonexistentRef Int -- Bad reference
                   | BadCode -- Error decoding something from an int to a datatype
                   | InternalError String -- Something else
                   | PayloadUnimplemented

instance Show DNSParseError where
  show (NonexistentRef i) = "Bad reference: " ++ show i
  show BadCode = "Bad code"
  show (InternalError s) = "Internal error: " ++ s
  show PayloadUnimplemented = "Payload unimplemented"

public
record DNSState : Type where
  MkDNSState : (blob : BufPtr) ->
               (labelCache : List (Position, List DomainFragment)) ->
               (pcktLen : Length) ->
               DNSState

public
data DNSParser : Effect where
  Initialise : BufPtr -> Length -> { () ==> DNSState } DNSParser ()
  Finalise : { DNSState ==> () } DNSParser ()
  ParseDNSPacket : (mkTy dns) -> { DNSState ==> () } DNSParser (Either DNSParseError DNSPacket)
  EncodeDNSPacket : DNSPacket -> { DNSState ==> () } DNSParser (mkTy dns)
  UpdateDNSState : DNSState -> { DNSState } DNSParser ()
  GetDNSState : { DNSState } DNSParser DNSState
  UnmarshalReference : DNSReference -> { DNSState } DNSParser (Maybe (List DomainFragment))

public
DNSPARSER : Type -> EFFECT
DNSPARSER t = MkEff t DNSParser



{-
parseDNSPacket : Applicative m =>
                 (mkTy dns) ->  
                 { [DNSPARSER DNSState] } 
                   Eff m (Either DNSParseError DNSPacket)
-}

packAndMap : Vect n (Bounded 8) -> String
packAndMap xs = pack $ map (chr . val) xs

unmarshalLabel : (mkTy dnsLabel) -> DomainFragment
unmarshalLabel (tagc ## len ## prf ## xs) = packAndMap xs

unmarshalReference' : BufPtr -> 
                      Position -> 
                      Length -> 
                      IO (Maybe (List DomainFragment))
unmarshalReference' pckt pos p_len = with Monad do
  let res = unmarshal' (ActivePacketRes pckt pos p_len) dnsLabels
  case res of
    Just (lbls ## _, res_len) => do 
      let res = map unmarshalLabel lbls
      return $ Just res
      --return $ Just (pack $ map (chr . val) xs) (doesn't work for some reason)
    Nothing => return Nothing

instance Handler DNSParser IO where
  handle st (UnmarshalReference ref) k = do
    putStrLn $ "Unmarshalling reference: " ++ (show ref)
    res <- unmarshalReference' (blob st) (ref * 8) (pcktLen st) 
    k res st
  handle _ (Initialise ptr len) k = k () (MkDNSState ptr [] len)
  handle _ (Finalise) k = k () ()
  handle st (GetDNSState) k = k st st
  handle st (UpdateDNSState st') k = k () st'

private
getDNSState : { [DNSPARSER DNSState] } Eff m DNSState
getDNSState = GetDNSState

private
updateDNSState : DNSState -> { [DNSPARSER DNSState] } Eff m ()
updateDNSState st = UpdateDNSState st

private
unmarshalReference : DNSReference -> 
                     { [DNSPARSER DNSState] } Eff m (Maybe (List DomainFragment))
unmarshalReference ref = UnmarshalReference ref

parseDNSHeader : (mkTy dnsHeader) -> DNSHeader
parseDNSHeader (ident ## qr ## opcode ## aa ## tc ## rd ## 
                ra ## z ## z_prf ## ans_auth ## nonauth_accept ## resp) = 
  MkDNSHeader (val ident) qr opcode aa tc rd ra ans_auth nonauth_accept resp
  

decodeReference : DNSReference -> { [DNSPARSER DNSState] } 
                  Eff m (Either DNSParseError (List DomainFragment))
decodeReference ref = do
  st <- getDNSState 
  case lookup ref (labelCache st) of
    Just frags => return $ Right frags
    Nothing => do
      unmarshal_res <- unmarshalReference ref
      return $ maybe (Left $ NonexistentRef ref) 
                     (\frags => Right frags) unmarshal_res

decodeLabels : (mkTy dnsLabels) -> 
               { [DNSPARSER DNSState] } 
                 Eff m (Either DNSParseError (List DomainFragment))
decodeLabels (lbls ## (Left null_term)) = return $ Right (map unmarshalLabel lbls)
decodeLabels (lbls ## (Right (_ ## ref))) = do
  let decoded_lbls = map unmarshalLabel lbls
  ref_lbls <- decodeReference (val ref)
  return $ either (\err => Left err)
                  (\r_lbls => Right $ decoded_lbls ++ r_lbls) ref_lbls


decodeDomain : (mkTy dnsDomain) -> 
               { [DNSPARSER DNSState] } 
                 Eff m (Either DNSParseError (List DomainFragment))
decodeDomain (Left (_ ## ref)) = decodeReference (val ref)
decodeDomain (Right encoded_lbls) = decodeLabels encoded_lbls

parseDNSQuestion : (mkTy dnsQuestion) -> { [DNSPARSER DNSState] } Eff m (Either DNSParseError DNSQuestion)
parseDNSQuestion (encoded_domain ## qt ## qc) = do
  decoded_domain <- decodeDomain encoded_domain
  case decoded_domain of
    Left err => return $ Left err
    Right decoded_domain' => 
      return $ Right (MkDNSQuestion decoded_domain' qt qc)

decodeIP : (mkTy dnsIP) -> SocketAddress
decodeIP (i1 ## i2 ## i3 ## i4) = 
  IPv4Addr (val i1) (val i2) (val i3) (val i4)

decodeNone : (mkTy null) -> ()
decodeNone _ = ()

decodeDomainPayload : (mkTy dnsDomain) -> 
                      { [DNSPARSER DNSState] }
                      Eff m (Either DNSParseError (DNSPayload DNSDomain))
decodeDomainPayload dom_pl = do
  domain <- decodeDomain dom_pl
  case domain of
    Left err => return $ Left err
    Right domain' => return $ Right (DNSDomainPayload domain')



getPayloadRel : (pl_ty : DNSPayloadType) ->
              (ty : DNSType) -> 
              (cls : DNSClass) -> 
              Either DNSParseError (DNSPayloadRel ty cls pl_ty)
getPayloadRel DNSIPv4 DNSTypeA DNSClassIN = Right DNSPayloadRelIP
getPayloadRel DNSIPv6 DNSTypeAAAA DNSClassIN = Right DNSPayloadRelIP6
getPayloadRel DNSDomain DNSTypeCNAME DNSClassIN = Right DNSPayloadRelCNAME
getPayloadRel DNSDomain DNSTypeNS DNSClassIN = Right DNSPayloadRelNS
getPayloadRel _ _ _ = Left PayloadUnimplemented


payloadType : DNSType -> DNSClass -> Either DNSParseError DNSPayloadType
payloadType DNSTypeA DNSClassIN = Right DNSIPv4
payloadType DNSTypeAAAA DNSClassIN = Right DNSIPv6
payloadType DNSTypeNS DNSClassIN =  Right DNSDomain
payloadType DNSTypeCNAME DNSClassIN = Right DNSDomain
payloadType _ _ = Left PayloadUnimplemented



total
decodePayload : (pl_rel : DNSPayloadRel ty cl pl_ty) ->
                (mkTy (dnsPayloadLang ty cl)) ->
                { [DNSPARSER DNSState] }
                Eff m (Either DNSParseError (DNSPayload pl_ty))
decodePayload DNSPayloadRelIP ip_pl = return $ Right (DNSIPv4Payload (decodeIP ip_pl))
decodePayload DNSPayloadRelCNAME dom_pl = decodeDomainPayload dom_pl
decodePayload DNSPayloadRelNS dom_pl = decodeDomainPayload dom_pl
decodePayload _ _ = return $ Left (PayloadUnimplemented)
 

-- TODO: Might be nice to prettify this... Pity we're wrapped in Eff so
-- we can't use the Either monad 
parseDNSRecord : (mkTy dnsRR) ->
                 { [DNSPARSER DNSState] } 
                 Eff m (Either DNSParseError DNSRecord)
parseDNSRecord (encoded_domain ## ty ## cls ## ttl ## len ## payload ## prf) = do
  let ttl' = val ttl
  domain <- (decodeDomain encoded_domain)
  case (payloadType ty cls) of
    Left err => return $ Left err
    Right pl_ty => do
      case (domain, getPayloadRel pl_ty ty cls) of
        (Left err, _) => return $ Left err
        (_, Left err) => return $ Left err
        (Right domain', Right pl_rel) => do 
          decoded_pl <- decodePayload pl_rel payload
          case decoded_pl of
            Left err => return $ Left err
            Right decoded_pl' => 
              return $ Right (MkDNSRecord domain' ty cls (val ttl) pl_rel decoded_pl')

-- Ugly hack, since records aren't of the same type and therefore we can't 
-- use sequence. Also proves to the TC that the lengths are as stated.
sequenceRecords : 
               Vect n (Either DNSParseError DNSRecord) -> 
               Vect m (Either DNSParseError DNSRecord) -> 
               Vect l (Either DNSParseError DNSRecord) -> 
               Either DNSParseError (Vect n DNSRecord, Vect m DNSRecord, Vect l DNSRecord)
sequenceRecords v1 v2 v3 = do
  v1' <- sequence v1
  v2' <- sequence v2
  v3' <- sequence v3
  return (v1', v2', v3')

parseDNSPacket : Applicative m =>
                 (mkTy dns) ->  
                 { [DNSPARSER DNSState] } 
                   Eff m (Either DNSParseError DNSPacket)
parseDNSPacket (hdr ## qdcount ## ancount ## nscount ## 
                arcount ## qs ## as ## auths ## additionals) = with Eff do
  let hdr' = parseDNSHeader hdr
  let n_qdcount = intToNat $ val qdcount
  let n_ancount = intToNat $ val ancount
  let n_nscount = intToNat $ val nscount
  let n_arcount = intToNat $ val arcount
  qs' <- mapVE parseDNSQuestion qs
  -- sequence results in TC not terminating
  -- let qs_ = sequence qs'
  let qs'' = sequence qs' 
  as' <- mapVE parseDNSRecord as
  auths' <- mapVE parseDNSRecord auths
  additionals' <- mapVE parseDNSRecord additionals
  let records = sequenceRecords as' auths' additionals'
  -- Now, see if everything was successful!
  case (qs'', records) of
    (Right qs''', Right (as'', auths'', additionals'')) => -- ?mv_rcase
      return $ Right (MkDNS hdr' _ _ _ _
                qs''' as'' auths'' additionals'')
    -- Record parsing may throw an error
    (Left err, _) => return $ Left err
    (_, Left err) => return $ Left err
    -- Nothing = bad code
    _ => return $ Left BadCode

--encodeDNSPacket : DNSPacket -> { [DNSPARSER DNSState] } Eff m (mkTy dns)
--encodeDNSPacket dnspckt = ?encodeDNSPacket_rhs

initialise : BufPtr -> Length -> { [DNSPARSER ()] ==> [DNSPARSER DNSState] } Eff IO ()
initialise ptr len = Initialise ptr len

finalise : { [DNSPARSER DNSState] ==> [DNSPARSER ()] } Eff IO ()
finalise = Finalise

public
parseDNS : BufPtr -> Length -> (mkTy dns) -> 
           { [DNSPARSER ()] } Eff IO (Either DNSParseError DNSPacket)
parseDNS ptr len pckt = with Eff do
  initialise ptr len
  res <- parseDNSPacket pckt
  finalise
  return res

public
data DNSEncodeError = OutOfBoundsError Nat Nat
                    | ProofConstructionError String
                    | LengthMismatchError Nat Nat
                    | UnsupportedPayloadType 
                    | InternalEncodeError String

instance Show DNSEncodeError where
  show (OutOfBoundsError n1 n2) = "Out of bounds: " ++ (show n1) ++ ", " ++ (show n2) 
  show (ProofConstructionError s) = "Proof construction error: " ++ s
  show (LengthMismatchError n1 n2) = "Length mismatch: " ++ (show n1) ++ ", " ++ (show n2)
  show UnsupportedPayloadType = "Unsupported payload type"
  show (InternalEncodeError s) = "Internal error: " ++ s

-- Our old friend... Might be a better way of doing this
lemma_vect_len : {x : Nat} -> (y : Nat) -> Vect x a -> Either DNSEncodeError (Vect y a)
lemma_vect_len {x} y xs with (decEq x y)
  lemma_vect_len {x} x xs | (Yes refl) = Right xs
  lemma_vect_len {x} y _  | (No _) = Left $ LengthMismatchError x y

---encodeVectLen : (n : Nat) -> Bounded

lemma_vect : (y : Bounded m) -> (xs : Vect n a) -> Either DNSEncodeError (Vect (intToNat (val y)) a )
lemma_vect b xs = lemma_vect_len (intToNat (val b)) xs  

isBounded : (bound : Nat) -> Nat -> Either DNSEncodeError (Bounded bound)
isBounded b n = 
  case choose (i_n < (pow 2 b)) of
      Left yes => Right (BInt i_n yes)
      Right _ => Left $ OutOfBoundsError b n
  where i_n : Int 
        i_n = natToInt n


zeroBit : Bounded 1
zeroBit = BInt 0 oh

oneBit : Bounded 1
oneBit = BInt 1 oh 

charToBits8 : Char -> Bounded 8
charToBits8 c = BInt (ord c) (believe_me oh) -- Premise

encodeString : String -> (n ** (Vect n (Bounded 8)))
encodeString str = 
  let unpacked_string = map charToBits8 (unpack str) in
  let vect_string = fromList unpacked_string in
  (_ ** vect_string)

mkTagCheck0 : (mkTy (tagCheck 0))
mkTagCheck0 = let zbit = zeroBit in
              (zbit ## zbit ## refl ## refl)

mkTagCheck1 : (mkTy (tagCheck 1))
mkTagCheck1 = let onebit = oneBit in
              (onebit ## onebit ## refl ## refl)

nullT : (mkTy nullterm)
nullT = (b0 ## oh)
  where b0 : Bounded 8
        b0 = BInt 0 oh


encodeDomainFragment : DomainFragment -> Either DNSEncodeError (mkTy dnsLabel)
encodeDomainFragment frag = with Monad do
  let (len ** vect_string) = encodeString frag
  encoded_len <- isBounded 6 len
  case choose ((val encoded_len) /= 0) of
    Left prf => 
      (lemma_vect encoded_len vect_string) >>= \encoded_string' =>
        Right (mkTagCheck0 ## encoded_len ## prf ## encoded_string') 
    Right _ => Left $ ProofConstructionError "Length of encoded domain fragment may not be zero"

encodeDomain : List DomainFragment -> Either DNSEncodeError (mkTy dnsDomain)
encodeDomain xs = with Monad do
  xs <- sequence $ map encodeDomainFragment xs
  return (Right (xs ## (Left nullT)))


encodeQuestion : DNSQuestion -> Either DNSEncodeError (mkTy dnsQuestion)
encodeQuestion (MkDNSQuestion qnames ty cls) = with Monad do
  dom <- encodeDomain qnames 
  return (dom ## ty ## cls)  

encodeHeader : DNSHeader -> Either DNSEncodeError (mkTy dnsHeader) 
encodeHeader (MkDNSHeader hdr_id query op auth trunc rd ra aa naa resp) = with Monad do
  b_id <- isBounded 16 (intToNat hdr_id)
  Right (b_id ## query ## op ## auth ## trunc ## rd ## 
        ra ## False ## oh ## aa ## naa ## resp)

encodeIP : DNSPayload DNSIPv4 -> Either DNSEncodeError (mkTy dnsIP)
encodeIP (DNSIPv4Payload (IPv4Addr i1 i2 i3 i4)) = with Monad do
  i1' <- isBounded 8 (intToNat i1)
  i2' <- isBounded 8 (intToNat i2)
  i3' <- isBounded 8 (intToNat i3)
  i4' <- isBounded 8 (intToNat i4)
  return (i1' ## i2' ## i3' ## i4')
-- TODO: If we parameterised SocketAddress over its type, we wouldn't have to do this.
encodeIP _ = Left $ InternalEncodeError "Attempted to encode ipv6 address using ipv4 function"



encodePayload : (rel : DNSPayloadRel ty cls pl_ty) -> 
                (payload : DNSPayload pl_ty) ->
                Either DNSEncodeError (mkTy (dnsPayloadLang ty cls))
--encodePayload {ty_code} {cls_code} rel ty_rel cls_rel = ?mv
encodePayload DNSPayloadRelIP payload = encodeIP payload
encodePayload DNSPayloadRelIP6 payload = Left UnsupportedPayloadType
encodePayload DNSPayloadRelCNAME (DNSDomainPayload payload) = encodeDomain payload
encodePayload DNSPayloadRelNS (DNSDomainPayload payload) = encodeDomain payload
encodePayload _ _ = Left UnsupportedPayloadType

{-
payloadLength : (rel : DNSPayloadRel ty cls pl_ty) ->
                (payload : (mkTy (dnsPayloadLang ty cls))) ->
                Either DNSEncodeError Int
payloadLength DNSPayloadRelIP pl = Right $ bitLength _ pl

payloadLength DNSPayloadRelIP6 pl = Left UnsupportedPayloadType
payloadLength DNSPayloadRelCNAME pl = encodeDomain payload
payloadLength DNSPayloadRelNS pl = encodeDomain payload
payloadLength _ _ = Left UnsupportedPayloadType
-}
%assert_total
encodeRR : DNSRecord -> Either DNSEncodeError (mkTy dnsRR)
encodeRR (MkDNSRecord name ty cls ttl rel pl) = with Monad do
  dom <- encodeDomain name
  b_ttl <- isBounded 32 (intToNat ttl)
  encoded_pl <- encodePayload rel pl
  let pl_len = (bitLength (dnsPayloadLang ty cls) encoded_pl) `div` 8
  b_len <- isBounded 16 (intToNat pl_len) 
  case decEq (val b_len) (bitLength (dnsPayloadLang ty cls) encoded_pl `div` 8) of
    Yes p =>
      Right (dom ## ty ## cls ## b_ttl ## b_len ## encoded_pl ## p)
    No _ => Left $ ProofConstructionError "Length field not equal to payload length"

public
encodeDNS : DNSPacket -> Either DNSEncodeError (mkTy dns) -- Maybe (mkTy dns)
encodeDNS (MkDNS hdr qc ac nsc arc qs as auths ars) = with Monad do
    encoded_hdr <- encodeHeader hdr
    b_qc <- isBounded 16 qc
    b_ac <- isBounded 16 ac
    b_nsc <- isBounded 16 nsc
    b_arc <- isBounded 16 arc 
    qs' <- sequence $ map encodeQuestion qs
    as' <- sequence $ map encodeRR as
    auths' <- sequence $ map encodeRR auths
    ars' <- sequence $ map encodeRR ars
    qs'' <- lemma_vect b_qc qs'
    as'' <- lemma_vect b_ac as'
    auths'' <- lemma_vect b_nsc auths' 
    ars'' <- lemma_vect b_arc ars' 
    return (encoded_hdr ## b_qc ## b_ac ## b_nsc ## b_arc ## qs'' ## as'' ## auths'' ## ars'')
 

-- Simple wrapper function to allow us to create a DNS request
public
mkDNSRequest : Int -> 
               List DomainFragment -> 
               DNSQType -> 
               DNSQClass -> 
               DNSPacket
mkDNSRequest req_id dom_frags qt qc = MkDNS hdr 1 0 0 0 [q] [] [] []
  where hdr = (MkDNSHeader req_id False QUERY False False 
              True True False False DNSResponseNoError)
        q = (MkDNSQuestion dom_frags qt qc)
-- runInit [(MkDNSState ptr [] len)] (parseDNSPacket pckt)

