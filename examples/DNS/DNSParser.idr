module DNSParser
import Network.Packet
import Network.PacketLang
import DNS
import Effects
import Effect.State

DNSReference : Type
DNSReference = Int

data DNSParseError = NonexistentRef Int -- Bad reference
                   | BadCode -- Error decoding something from an int to a datatype
                   | InternalError String -- Something else
                   | PayloadUnimplemented
record DNSState : Type where
  MkDNSState : (blob : RawPacket) ->
               (labelCache : List (Position, List DomainFragment)) ->
               (pcktLen : Length) ->
               DNSState


data DNSParser : Effect where
  ParseDNSPacket : (mkTy dns) -> Length -> { DNSState } DNSParser (Either DNSParseError DNSPacket)
  EncodeDNSPacket : DNSPacket -> { DNSState } DNSParser (mkTy dns)
  UpdateDNSState : DNSState -> { DNSState } DNSParser ()
  GetDNSState : { DNSState } DNSParser DNSState
  UnmarshalReference : DNSReference -> { DNSState } DNSParser (Maybe (List DomainFragment))

DNSPARSER : Type -> EFFECT
DNSPARSER t = MkEff t DNSParser


packAndMap : Vect n (Bounded 8) -> String
packAndMap xs = pack $ map (chr . val) xs

unmarshalLabel : (mkTy dnsLabel) -> DomainFragment
unmarshalLabel (tagc ## len ## prf ## xs) = packAndMap xs

unmarshalReference' : RawPacket -> 
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
    res <- unmarshalReference' (blob st) (pcktLen st) ref
    k res st

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
                ra ## z ## z_prf ## resp ## resp_prf) = do
  op <- codeToDNSOpcode $ (val opcode)
  resp' <- dnsCodeToResponse $ (val resp)
  return $ MkDNSHeader (val ident) qr op aa tc rd ra resp'
  

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
parseDNSQuestion (encoded_domain ## qt ## qt_prf ## qc ## qc_prf) = ?mv
{- do
  decoded_domain <- decodeDomain encoded_domain
  case (decoded_domain, codeToDNSQType (val qt), codeToDNSQClass (val qc)) of
    (Right domain', Just qt', Just qc') => return $ Just (MkDNSQuestion domain' qt' qc')
    _ => return Nothing
-}

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


encodeDNSPacket : DNSPacket -> { [DNSPARSER DNSState] } Eff m (mkTy dns)
encodeDNSPacket dnspckt = ?encodeDNSPacket_rhs


