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
unmarshalReference' pckt pos p_len = do
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

parseDNSHeader : (mkTy dnsHeader) -> Maybe DNSHeader
parseDNSHeader (ident ## qr ## opcode ## opcode_prf ## aa ## tc ## rd ## 
                ra ## z ## z_prf ## ans_auth ## nonauth_accept ## resp ## resp_prf) = do
  op <- codeToDNSOpcode $ (val opcode)
  resp' <- dnsCodeToResponse $ (val resp)
  return $ MkDNSHeader (val ident) qr op aa tc rd ra ans_auth nonauth_accept resp'
  

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

parseDNSQuestion : (mkTy dnsQuestion) -> { [DNSPARSER DNSState] } Eff m (Maybe DNSQuestion)
parseDNSQuestion (encoded_domain ## qt ## qt_prf ## qc ## qc_prf) = do
  decoded_domain <- decodeDomain encoded_domain
  case (decoded_domain, dnsCodeToQType (val qt), dnsCodeToQClass (val qc)) of
    (Right domain', Just qt', Just qc') => return $ Just (MkDNSQuestion domain' qt' qc')
    _ => return Nothing


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

total
decodePayload : (pl_rel : DNSPayloadRel ty cl pl_ty) ->
                (ty_rel : DNSTypeRel i_ty ty) ->
                (cl_rel : DNSClassRel i_cl cl) ->
                (mkTy (dnsPayloadLang i_ty i_cl)) ->
                { [DNSPARSER DNSState] }
                Eff m (Either DNSParseError (DNSPayload pl_ty))
decodePayload DNSPayloadRelIP DNSTypeRelA DNSClassRelIN ip_pl = return $ Right (DNSIPv4Payload (decodeIP ip_pl))
decodePayload DNSPayloadRelCNAME DNSTypeRelCNAME DNSClassRelIN dom_pl = decodeDomainPayload dom_pl
decodePayload DNSPayloadRelNS DNSTypeRelNS DNSClassRelIN dom_pl = decodeDomainPayload dom_pl
decodePayload _ _ _ _ = return $ Left (PayloadUnimplemented)


getRelations : (ty_code : Int) -> 
               (cls_code : Int) -> 
               (ty : DNSType) ->
               (cls : DNSClass) ->
               (pl_ty : DNSPayloadType) ->
               Maybe (DNSTypeRel ty_code ty, DNSClassRel cls_code cls, DNSPayloadRel ty cls pl_ty)
getRelations ty_code cls_code ty cls pl_ty = do
  t_rel <- dnsTypeRel ty_code ty
  c_rel <- dnsClassRel cls_code cls
  pl_rel <- getPayloadRel pl_ty ty cls
  return (t_rel, c_rel, pl_rel)


decodeRecordCodes : (ty_code : Int) -> (cls_code : Int) -> Maybe (DNSType, DNSClass, DNSPayloadType)
decodeRecordCodes ty_code cls_code = do
  ty <- dnsCodeToType ty_code
  cls <- dnsCodeToClass cls_code
  pl_ty <- payloadType ty cls
  return (ty, cls, pl_ty)

parseDNSRecord : (mkTy dnsRR) ->
                 { [DNSPARSER DNSState] } 
                 Eff m (Either DNSParseError DNSRecord)
parseDNSRecord (encoded_domain ## ty ## ty_prf ## cls ## cls_prf ## ttl ## len ## len_prf ## payload) = do
  let ttl' = val ttl
  domain <- (decodeDomain encoded_domain)
  case decodeRecordCodes (val ty) (val cls) of
    Just (ty', cls', pl_ty) => do
      let m_rels = getRelations (val ty) (val cls) ty' cls' pl_ty
      case m_rels of
        Just (ty_rel, cls_rel, pl_rel) => do
          payload' <- decodePayload pl_rel ty_rel cls_rel payload
          case (domain, payload') of 
            (Left err, _) => return $ Left err
            (_, Left err) => return $ Left err
            (Right domain'', Right payload'') => return $ Right (MkDNSRecord domain'' ty' cls' ttl' pl_rel payload'')
    Nothing => return $ Left PayloadUnimplemented


-- FIXME: For some reason, sequence isn't working (causing infinite TC loop)
-- so here's a specialised version...
sequenceRecord : Vect n (Either DNSParseError DNSRecord) ->
                 Either DNSParseError (Vect n DNSRecord)
sequenceRecord [] = Right []
sequenceRecord ((Left err) :: _) = Left err
sequenceRecord ((Right rec) :: recs) = sequenceRecord recs >>= (\recs' => Right (rec :: recs'))

sequenceQuestion : Vect n (Maybe DNSQuestion) ->
                   Maybe (Vect n DNSQuestion)
sequenceQuestion [] = Just []
sequenceQuestion (Nothing :: _) = Nothing
sequenceQuestion ((Just rec) :: recs) = sequenceQuestion recs >>= (\recs' => Just (rec :: recs'))




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
                arcount ## qs ## as ## auths ## additionals) = do
  let hdr' = parseDNSHeader hdr
  let n_qdcount = intToNat $ val qdcount
  let n_ancount = intToNat $ val ancount
  let n_nscount = intToNat $ val nscount
  let n_arcount = intToNat $ val arcount
  qs' <- mapVE parseDNSQuestion qs
  -- sequence results in TC not terminating
  -- let qs_ = sequence qs'
  let qs'' = sequenceQuestion qs' 
  as' <- mapVE parseDNSRecord as
  auths' <- mapVE parseDNSRecord auths
  additionals' <- mapVE parseDNSRecord additionals
  let records = sequenceRecords as' auths' additionals'
  -- Now, see if everything was successful!
  case (hdr', qs'', records) of
    (Just hdr'', Just qs''', Right (as'', auths'', additionals'')) => -- ?mv_rcase
      return $ Right (MkDNS hdr'' _ _ _ _
                qs''' as'' auths'' additionals'')
    -- Record parsing may throw an error
    (_, _, Left err) => return $ Left err
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
parseDNS ptr len pckt = do
  initialise ptr len
  res <- parseDNSPacket pckt
  finalise
  return res

-- data DNSEncodeError = SomeEncodeError



-- Our old friend... Might be a better way of doing this
lemma_vect_len : {x : Nat} -> (y : Nat) -> Vect x a -> Maybe (Vect y a)
lemma_vect_len {x} y xs with (decEq x y)
  lemma_vect_len {x} x xs | (Yes refl) = Just xs
  lemma_vect_len {x} y _  | (No _) = Nothing

---encodeVectLen : (n : Nat) -> Bounded

lemma_vect : {m : Nat} -> (xs : Vect n a) -> (y : Bounded m) -> Maybe (Vect (intToNat (val y)) a )
lemma_vect {m} xs b = lemma_vect_len (intToNat (val b)) xs  

isBounded : (bound : Nat) -> Nat -> (Maybe (Bounded bound))
isBounded b n = 
  case choose (i_n < (pow 2 b)) of
      Left yes => Just (BInt i_n yes)
      Right _ => Nothing
  where i_n : Int 
        i_n = natToInt n


encodeHeader : DNSHeader -> (mkTy dnsHeader) 
encodeHeader (MkDNSHeader hdr_id query op auth trunc rd ra aa naa resp) =  ?encodeHeader_rhs

encodeRR : DNSRecord -> (mkTy dnsRR)
encodeRR (MkDNSRecord name ty cls ttl rel pl) = ?encodeRR_rhs

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

--mkTagCheck0 : (mkTy (tagCheck 0))
--mkTagCheck0 = (zeroBit ## zeroBit ## (Yes refl) ## (Yes refl))

{-
mkTagCheck0 : (mkTy (tagCheck 0))
mkTagCheck0 with (decEq (val zeroBit) 0)
  mkTagCheck0 | Yes refl = (zeroBit ## zeroBit ## refl ## refl)
  mkTagCheck0 | No refl impossible
--  mkTagCheck x 0 | No _ = (zeroBit ## zeroBit ## refl ## refl)
-}

mkTagCheck0 : (mkTy (tagCheck 0))
mkTagCheck0 = let zbit = zeroBit in
              (zbit ## zbit ## refl ## refl)
--mkTagCheck1 : (mkTy (tagCheck 1))
--mkTagCheck1 = (oneBit ## oneBit ## refl ## refl)

nullT : (mkTy nullterm)
nullT = (b0 ## oh)
  where b0 : Bounded 8
        b0 = BInt 0 oh

encodeDomainFragment : DomainFragment -> Maybe (mkTy dnsLabel)
encodeDomainFragment frag = do
  let (len ** vect_string) = encodeString frag
  encoded_len <- isBounded 6 len
  case choose ((val encoded_len) /= 0) of
    Left prf => 
      case lemma_vect vect_string encoded_len of
        Just encoded_string' => Just (mkTagCheck0 ## encoded_len ## prf ## encoded_string') 
        Nothing => Nothing
    _ => Nothing

encodeDomain : List DomainFragment -> Maybe (mkTy dnsDomain)
encodeDomain xs = do
  xs <- sequence $ map encodeDomainFragment xs
  return (Right (xs ## (Left nullT)))


encodeQuestion : DNSQuestion -> Maybe (mkTy dnsQuestion)
encodeQuestion (MkDNSQuestion qnames ty cls) = do
  dom <- encodeDomain qnames 
  b_ty_code <- isBounded 16 (intToNat $ dnsQTypeToCode ty)
  b_cls_code <- isBounded 16 (intToNat $ dnsQClassToCode cls) 
  case (choose (validQTYPE (val b_ty_code)), choose (validQCLASS (val b_cls_code))) of
    (Left p1, Left p2) => Just (dom ## b_ty_code ## p1 ## b_cls_code ## p2)
    _ => Nothing
{-
encodeDNS : DNSPacket -> Maybe (mkTy dns)
encodeDNS (MkDNS hdr qc ac nsc arc qs as auths ars) = do
    b_qc <- isBounded 16 qc
    qs'' <- lemma_vect (map encodeQuestion qs) b_qc
    b_ac <- isBounded 16 ac
    ac'' <- lemma_vect (map encodeRR as) b_ac
    b_nsc <- isBounded 16 nsc
    nsc'' <- lemma_vect (map encodeRR auths) b_nsc
    b_arc <- isBounded 16 arc 
    arc'' <- lemma_vect (map encodeRR ars) b_arc
    Just ((encodeHeader hdr) ## b_qc ## b_ac ## b_nsc ## b_arc ## qs'' ## ac'' ## nsc'' ## arc'')
 
   ((encodeHeader hdr) ## _ ## _ ## _ ## _ ## 
     (map encodeQuestion qs) ## (map encodeRR as) ## 
     (map encodeRR auths) ## (map encodeRR ars))-- encoded_ars)

  where encoded_header : (mkTy dnsHeader)
        encoded_header = encodeHeader hdr
        encoded_qs : Maybe (Vect (intToNat (val qs_n)) DNSQuestion)
        encoded_qs = lemma_vect_n 
-}
-- runInit [(MkDNSState ptr [] len)] (parseDNSPacket pckt)


