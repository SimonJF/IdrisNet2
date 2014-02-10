module DNSParser
import Network.Packet
import Network.PacketLang
import DNS
import Effects
import Effect.State

DNSReference : Type
DNSReference = Int

data DNSParseError = NonexistentRef Int -- Bad reference
                   | BadCode Int -- Error decoding something from an int to a datatype

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
  op <- fromCode $ val opcode
  resp' <- fromCode $ val resp
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
parseDNSQuestion (encoded_domain ## qt ## qt_prf ## qc ## qc_prf) = do
  decoded_domain <- decodeDomain encoded_domain
  ?mv_parseq

parseDNSPacket : (mkTy dns) ->  
                 { [DNSPARSER DNSState] } 
                   Eff m (Either DNSParseError DNSPacket)
parseDNSPacket (hdr ## qdcount ## ancount ## nscount ## 
                arcount ## qs ## as ## auths ## additionals) = do
  let dns_header = parseDNSHeader hdr
  ?mv_pckt
--parseDNSPacket (hdr ## qdcount ## ancount ## nscount ## arcount ## qs ## as ## auths ## additionals) = ?mv

encodeDNSPacket : DNSPacket -> { [DNSPARSER DNSState] } Eff m (mkTy dns)
encodeDNSPacket dnspckt = ?encodeDNSPacket_rhs


