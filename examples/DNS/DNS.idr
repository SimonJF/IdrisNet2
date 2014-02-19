-- PacketLang implementation of DNS packets.
-- Also data representations of DNS packet.

-- PacketLang representations:
module DNS

import Network.PacketLang
import Network.Socket
import DNSCodes

%access public



-- Data representations of DNS Packets

-- DNS Types:
-- These are record types, so for example CNAME or MX.
-- Types are basic ones, but QTypes are a superset.



data DNSQuestion : Type where
  MkDNSQuestion : (dnsQNames : List DomainFragment) ->
                  (dnsQQType : DNSQType) ->
                  (dnsQQClass : DNSQClass) ->
                  DNSQuestion

data DNSRecord : Type where
  MkDNSRecord : (dnsRRName : List DomainFragment) ->
                (dnsRRType : DNSType) ->
                (dnsRRClass : DNSClass) ->
                (dnsRRTTL : Int) ->
                (dnsRRPayload : payloadType dnsRRType dnsRRClass) ->
                DNSRecord



record DNSHeader : Type where
  MkDNSHeader : (dnsHdrId : Int) ->
                (dnsHdrIsQuery : Bool) ->
                (dnsHdrOpcode : DNSHdrOpcode) ->
                (dnsHdrIsAuthority : Bool) ->
                (dnsHdrIsTruncated : Bool) ->
                (dnsHdrRecursionDesired : Bool) ->
                (dnsHdrRecursionAvailable : Bool) ->
                (dnsHdrResponse : DNSResponse) ->
                DNSHeader

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

abstract
nullterm : PacketLang
nullterm = do nt <- bits 8
              check ((val nt) == 0)

validRespCode : Int -> Bool
validRespCode i = i >= 0 && i <= 5

-- DNS allows compression in the form of references.
-- These take the form of two octets, the first two bits of which 
-- are 11. The rest is 14 bits.
abstract -- This is junk, we don't really want it to reduce
tagCheck : Int -> PacketLang
tagCheck i = do tag1 <- bits 1
                tag2 <- bits 1
                let v1 = val tag1
                let v2 = val tag2
                prop_eq v1 i
                prop_eq v2 i

dnsReference : PacketLang
dnsReference = do tagCheck 1
                  bits 14

 --                 check (((val tag1) == 1) && ((val tag2) == 1))

dnsLabel : PacketLang
dnsLabel = do tagCheck 0
              len <- bits 6
              let vl = (val len)
              prf <- check (vl /= 0) 
              listn (intToNat vl) (bits 8)


dnsLabels : PacketLang
dnsLabels = do list dnsLabel
               nullterm // dnsReference

dnsDomain : PacketLang
dnsDomain = dnsReference // dnsLabels

dnsQuestion : PacketLang
dnsQuestion = do dnsDomain
                 qtype <- bits 16
                 check (validQTYPE (val qtype))
                 qclass <- bits 16 
                 check (validQCLASS 16)

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
dnsPayloadLang : (ty : Int) -> (cls : Int) -> PacketLang
dnsPayloadLang 1 1 = dnsIP
dnsPayloadLang 2 1 = dnsDomain
dnsPayloadLang 5 1 = dnsDomain
dnsPayloadLang 28 1 = null
dnsPayloadLang _ _ = null

-- abstract
dnsHeader : PacketLang
dnsHeader = 
  with PacketLang do 
     ident <- bits 16 -- Request identifier
     qr <- bool -- Query or response 
     opcode <- bits 4 -- Which type of query? Only 0, 1 and 2 valid
     check (validOpcode (val opcode))
     aa <- bool -- Only set in response, is responding server authority?
     tc <- bool -- Was message truncated?
     rd <- bool -- Recursion desired, set in query, copied into response
     ra <- bool -- Recursion available; is support available in NS?
     z  <- bool -- Must be 0.
     check (not z)
     resp <- bits 4 -- Response code, only 0-5 valid
     check (validRespCode (val resp))

-- DNS Resource Record
-- The same for answers, authorities and additional info.
dnsRR : PacketLang
dnsRR = with PacketLang do 
           domain <- dnsDomain
           ty <- bits 16
           let vt = val ty
           check (validTYPE vt)
           cls <- bits 16
           let vc = val cls
           check (validCLASS vc)
           ttl <- bits 32
           len <- bits 16 -- Length in octets of next field
           let vl = ((val len) * 8)
           prf <- check (vl > 0)
           dnsPayloadLang vt vc
--           bounded_bits vl prf

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


