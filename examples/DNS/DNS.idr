-- PacketLang implementation of DNS packets.
-- Also data representations of DNS packet.

-- PacketLang representations:
module DNS

import Network.PacketLang
import Network.Socket
import DNSCodes

%access public
%default total


-- Data representations of DNS Packets

-- DNS Types:
-- These are record types, so for example CNAME or MX.
-- Types are basic ones, but QTypes are a superset.



data DNSQuestion : Type where
  MkDNSQuestion : (dnsQNames : List DomainFragment) ->
                  (dnsQQType : DNSQType) ->
                  (dnsQQClass : DNSQClass) ->
                  DNSQuestion

instance Show DNSQuestion where
  show (MkDNSQuestion qns qqt qqc) =
    "DNS Question: \n" ++ 
    "DNS QNames: \n" ++ (show qns) ++ "\n" ++
    "DNS QType: \n" ++ (show qqt) ++ "\n" ++
    "DNS QClass: \n" ++ (show qqc) ++ "\n"


data DNSRecord : Type where
  MkDNSRecord : (dnsRRName : List DomainFragment) ->
                (dnsRRType : DNSType) ->
                (dnsRRClass : DNSClass) ->
                (dnsRRTTL : Int) ->
                (dnsRRRel : DNSPayloadRel dnsRRType dnsRRClass pl_ty) -> 
                (dnsRRPayload : DNSPayload pl_ty) ->
                DNSRecord

instance Show DNSRecord where
  show (MkDNSRecord name ty cls ttl rel pl) = "DNS Record: \n" ++
    "DNS RR Name: " ++ (show name) ++ "\n" ++
    "DNS RR Type: " ++ (show ty) ++ "\n" ++
    "DNS RR Class: " ++ (show cls) ++ "\n" ++
    "DNS RR TTL: " ++ (show ttl) ++ "\n" ++
    "DNS RR Payload: " ++ (showPayload rel pl) ++ "\n"

record DNSHeader : Type where
  MkDNSHeader : (dnsHdrId : Int) ->
                (dnsHdrIsQuery : Bool) ->
                (dnsHdrOpcode : DNSHdrOpcode) ->
                (dnsHdrIsAuthority : Bool) ->
                (dnsHdrIsTruncated : Bool) ->
                (dnsHdrRecursionDesired : Bool) ->
                (dnsHdrRecursionAvailable : Bool) ->
                (dnsAnswerAuthenticated : Bool) ->
                (dnsNonAuthAcceptable : Bool) ->
                (dnsHdrResponse : DNSResponse) ->
                DNSHeader

instance Show DNSHeader where
  show (MkDNSHeader hdr_id query op auth trunc rd ra aa naa resp) = 
    "DNS Header\n" ++ 
    "ID : " ++ (show hdr_id) ++ "\n" ++ 
    "Is query? : " ++ (show query) ++ "\n" ++ 
    "Opcode : " ++ (show op) ++ "\n" ++ 
    "Is authority? : " ++ (show auth) ++ "\n" ++ 
    "Is truncated? : " ++ (show trunc) ++ "\n" ++
    "Is recursion desired? : " ++ (show rd) ++ "\n" ++
    "Is recursion available? : " ++ (show ra) ++ "\n" ++
    "Is answer authenticated? : " ++ (show aa) ++ "\n" ++
    "Is non-authenticated data acceptable? : " ++ (show naa) ++ "\n" ++
    "Response : " ++ (show resp) ++ "\n"
    

record DNSPacket : Type where
  MkDNS : (dnsPcktHeader : DNSHeader) -> 
          (dnsPcktQDCount : Nat) ->
          (dnsPcktANCount : Nat) ->
          (dnsPcktNSCount : Nat) ->
          (dnsPcktARCount : Nat) ->
          (dnsPcktQuestions : Vect dnsPcktQDCount DNSQuestion) ->
          (dnsPcktAnswers : Vect dnsPcktANCount DNSRecord) ->
          (dnsPcktAuthorities : Vect dnsPcktNSCount DNSRecord) ->
          (dnsPcktAdditionals : Vect dnsPcktARCount DNSRecord) ->
          DNSPacket

instance Show DNSPacket where
  show (MkDNS hdr qdc anc nsc arc qs as auths adds) = 
    "DNS Packet: " ++ "\n" ++
    "QD Count: " ++ (show qdc) ++ "\n" ++
    "AN Count: " ++ (show anc) ++ "\n" ++
    "NS Count: " ++ (show nsc) ++ "\n" ++
    "AR Count: " ++ (show arc) ++ "\n" ++
    "Questions: " ++ (show qs) ++ "\n" ++
    "Answers: " ++ (show as) ++ "\n" ++
    "Authorities: " ++ (show auths) ++ "\n" ++
    "Additionals: " ++ (show adds) ++ "\n"
  

--instance Show DNSPacket where
  --show (MkDNS hdr qd an ns ar qs as auths adds) = 
    -- "DNS Packet: \n" ++
    
-- Verified implementation of the DNS packet specification

-- Validation of TYPE and QTYPE fields
validTYPE : Int -> Bool
validTYPE i = i >= 0 && i <= 16

validQTYPE : Int -> Bool
validQTYPE i = (validTYPE i) || (i >= 252 && i <= 255)

-- Validation of CLASS fields
validCLASS : Int -> Bool
validCLASS i = i >= 1 || i <= 4 -- In practice, this will only be 1..

validQCLASS : Int -> Bool
validQCLASS i = (validCLASS i) || i == 255

validOpcode : Int -> Bool
validOpcode i = i == 0 || i == 1 || i == 2

nullterm : PacketLang
nullterm = do nt <- bits 8
              check ((val nt) == 0)

validRespCode : Int -> Bool
validRespCode i = i >= 0 && i <= 5

-- DNS allows compression in the form of references.
-- These take the form of two octets, the first two bits of which 
-- are 11. The rest is 14 bits.
tagCheck : Int -> PacketLang
tagCheck i = do tag1 <- bits 1
                tag2 <- bits 1
                let v1 = val tag1
                let v2 = val tag2
                prop (prop_eq v1 i)
                prop (prop_eq v2 i)

dnsReference : PacketLang
dnsReference = do tagCheck 1
                  bits 14

dnsLabel: PacketLang
dnsLabel = do tagCheck 0
              len <- bits 6
              let vl = (val len)
              check (vl /= 0)
              listn (intToNat vl) (bits 8)


dnsLabels : PacketLang
dnsLabels = do list dnsLabel
               nullterm // dnsReference

dnsDomain : PacketLang
dnsDomain = dnsReference // dnsLabels

dnsQuestion : PacketLang
dnsQuestion = do dnsDomain
                 decodable 16 DNSQType dnsCodeToQType dnsQTypeToCode
                 decodable 16 DNSQClass dnsCodeToQClass dnsQClassToCode

dnsIP : PacketLang
dnsIP = do
  bits 8
  bits 8
  bits 8
  bits 8

{-  Pending compiler bugfix...
dnsPayloadLang : (ty : Int) -> (cls : Int) -> PacketLang
dnsPayloadLang A_VAL IN_VAL = dnsIP
dnsPayloadLang CNAME_VAL IN_VAL = dnsDomain
dnsPayloadLang NS_VAL IN_VAL = dnsDomain
dnsPayloadLang AAAA_VAL IN_VAL = null
dnsPayloadLang _ _ = null
-}

dnsPayloadLang : DNSType -> DNSClass -> PacketLang
dnsPayloadLang DNSTypeA DNSClassIN = dnsIP
dnsPayloadLang DNSTypeAAAA DNSClassIN = null 
dnsPayloadLang DNSTypeNS DNSClassIN = dnsDomain
dnsPayloadLang DNSTypeCNAME DNSClassIN = dnsDomain
dnsPayloadLang _ _ = null



dnsHeader : PacketLang
dnsHeader = 
  with PacketLang do 
     ident <- bits 16 -- Request identifier
     qr <- bool -- Query or response 
     opcode <- decodable 4 DNSHdrOpcode dnsCodeToOpcode dnsOpcodeToCode
     aa <- bool -- Only set in response, is responding server authority?
     tc <- bool -- Was message truncated?
     rd <- bool -- Recursion desired, set in query, copied into response
     ra <- bool -- Recursion available; is support available in NS?
     z  <- bool -- Must be 0.
     check (not z)
     ans_auth <- bool
     auth_acceptable <- bool
     decodable 4 DNSResponse dnsCodeToResponse dnsResponseToCode

-- DNS Resource Record
-- The same for answers, authorities and additional info.
%assert_total
dnsRR : PacketLang
dnsRR = with PacketLang do 
           domain <- dnsDomain
           ty <- decodable 16 DNSType dnsCodeToType' dnsTypeToCode'
           cls <- decodable 16 DNSClass dnsCodeToClass' dnsClassToCode' 
           ttl <- bits 32
           len <- bits 16 -- Length in octets of next field
--            prf <- check ((val len) > 0)
           let pl_lang = dnsPayloadLang ty cls
           pl_data <- pl_lang
           let data_len = (bitLength pl_lang pl_data) `div` 8
           prop (prop_eq (val len) data_len)


dns : PacketLang
dns = with PacketLang do 
         header <- dnsHeader
         -- Technically these are parts of the header, but we 
         -- need them in scope for later
         qdcount <- bits 16 -- Number of entries in question section
         ancount <- bits 16 -- Number of entries in the answer section
         nscount <- bits 16 -- Number of NS resource records in auth records
         arcount <- bits 16 -- Number of resource records in additional records section
         questions <- listn (intToNat $ val qdcount) dnsQuestion 
         answers <- listn (intToNat $ val ancount) dnsRR
         authorities <- listn (intToNat $ val nscount) dnsRR
         listn (intToNat $ val arcount) dnsRR -- Additional


