-- PacketLang implementation of DNS packets.
-- Also data representations of DNS packet.

-- PacketLang representations:
module DNS

import Network.PacketLang

%access public

DomainFragment : Type
DomainFragment = String

-- Data representations of DNS Packets

-- DNS Types:
-- These are record types, so for example CNAME or MX.
-- Types are basic ones, but QTypes are a superset.

class Code a where
  toCode : a -> Int
  fromCode : Int -> Maybe a


data DNSHdrOpcode = QUERY | IQUERY | STATUS

dnsOpcodeToCode : DNSHdrOpcode -> Int
dnsOpcodeToCode QUERY = 0
dnsOpcodeToCode IQUERY = 1
dnsOpcodeToCode STATUS = 2

codeToDNSOpcode : Int -> Maybe DNSHdrOpcode
codeToDNSOpcode i = lookup i (the (List _) [(0, QUERY), (1, IQUERY), (2, STATUS)])

instance Code DNSHdrOpcode where
  toCode x = dnsOpcodeToCode x
  fromCode i = codeToDNSOpcode i

data DNSClass = IN -- Internet
              | CS  -- CSNET
              | CH -- CHAOS
              | HS -- Hesiod

data DNSQClass = AnyClass 
               | DNSBaseClass DNSClass

dnsClassToCode : DNSClass -> Int
dnsClassToCode IN = 1
dnsClassToCode CS = 2
dnsClassToCode CH = 3
dnsClassToCode HS = 4

codeToDNSClass : Int -> Maybe DNSClass
codeToDNSClass i = 
  lookup i (the (List (Int, DNSClass)) [(1, IN), (2, CS), (3, CH), (4, HS)])

dnsQClassToCode : DNSQClass -> Int
dnsQClassToCode AnyClass = 255
dnsQClassToCode (DNSBaseClass x) = dnsClassToCode x

codeToDNSQClass : Int -> Maybe DNSQClass
codeToDNSQClass 255 = Just AnyClass
codeToDNSQClass i = map DNSBaseClass (codeToDNSClass i)

-- Type descriptions taken from RFC1035
data DNSType = A -- A host address
             | NS -- An authoritative name server
             | MD -- A mail destination (obsolete, use MX)
             | MF -- A mail forwarder (obsolete, use MX)
             | CNAME -- Canonical name for an alias
             | SOA -- Start of authority
             | MB -- Mailbox domain name
             | MG -- Mail group member
             | MR -- Mail rename domain name
             | NULL -- Null RR
             | WKS -- Well-known service description
             | PTR -- A domain name pointer
             | HINFO -- Host information
             | MINFO -- Mailbox or mail list informatio n
             | MX -- Mail exchange
             | TXT -- Text record

data DNSQType = AXFR -- Request for transfer of entire zone
              | MAILB -- Request for mailbox-related records
              | MAILA -- Request for mail agent RRs (obsolete, use MX) 
              | ALL -- Request for all records
              | DNSBaseTy DNSType -- Since this is a superset, all DNSTypes
                                  -- are also valid

{- TODO (possibly?): make this a tad cleaner -}

dnsTypeToCode : DNSType -> Int
dnsTypeToCode A = 1
dnsTypeToCode NS = 2
dnsTypeToCode MD = 3
dnsTypeToCode MF = 4
dnsTypeToCode CNAME = 5
dnsTypeToCode SOA = 6
dnsTypeToCode MB = 7
dnsTypeToCode MG = 8
dnsTypeToCode MR = 9
dnsTypeToCode NULL = 10
dnsTypeToCode WKS = 11
dnsTypeToCode PTR = 12
dnsTypeToCode HINFO = 13
dnsTypeToCode MINFO = 14
dnsTypeToCode MX = 15
dnsTypeToCode TXT = 16

codeToDNSType : Int -> Maybe DNSType
codeToDNSType i = 
  lookup i (the (List (Int, DNSType)) 
            [(1, A), (2, NS), (3, MD), (4, MF), (5, CNAME), (6, SOA), (7, MB), 
             (8, MG), (9, MR), (10, NULL), (11, WKS), (12, PTR), (13, HINFO), 
             (14, MINFO), (15, MX), (16, TXT)
            ])

dnsQTypeToCode : DNSQType -> Int
dnsQTypeToCode AXFR = 252
dnsQTypeToCode MAILB = 253 
dnsQTypeToCode MAILA = 254 
dnsQTypeToCode ALL = 255
dnsQTypeToCode (DNSBaseTy ty) = dnsTypeToCode ty

codeToDNSQType : Int -> Maybe DNSQType
codeToDNSQType 252 = Just AXFR
codeToDNSQType 253 = Just MAILB
codeToDNSQType 254 = Just MAILA
codeToDNSQType 255 = Just ALL
codeToDNSQType i = map DNSBaseTy (codeToDNSType i)

instance Code DNSType where
  toCode x = dnsTypeToCode x
  fromCode i = codeToDNSType i

instance Code DNSQType where
  toCode x = dnsQTypeToCode x
  fromCode i = codeToDNSQType i

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
                (dnsRRPayload : Int) -> -- FIXME Temp, haven't decided how to do this yet
                DNSRecord

data DNSResponse = DNSNoError
                 | DNSFormatError
                 | DNSServerError
                 | DNSNameError
                 | DNSNotImplementedError
                 | DNSRefusedError
                 

dnsResponseToCode : DNSResponse -> Int
dnsResponseToCode DNSNoError = 0
dnsResponseToCode DNSFormatError = 1
dnsResponseToCode DNSServerError = 2 
dnsResponseToCode DNSNameError = 3 
dnsResponseToCode DNSNotImplementedError = 4 
dnsResponseToCode DNSRefusedError = 5

codeFromDNSResponse : Int -> Maybe DNSResponse
codeFromDNSResponse i = 
  lookup i (the (List _ ) [(0, DNSNoError), (1, DNSFormatError), (2, DNSServerError), 
            (3, DNSNameError), (4, DNSNotImplementedError), (5, DNSRefusedError)])

instance Code DNSResponse where
  toCode = dnsResponseToCode
  fromCode = codeFromDNSResponse

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
          (dnsPcktQuestions : List DNSQuestion) ->
          (dnsPcktAnswers : List DNSRecord) ->
          (dnsPcktAdditionals : List DNSRecord) ->
          DNSPacket
{-
          TODO: These would be nice...
          (dnsPcktQuestions : Vect dnsPcktQDCount DNSQuestion) ->
          (dnsPcktAnswers : Vect dnsPcktANCount DNSRecord) ->
          (dnsPcktAuthorities : Vect dnsPcktNSCount DNSRecord) ->
          (dnsPcktAdditionals : Vect dnsPcktNSCount DNSRecord) ->
          DNSPacket
-}
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

-- abstract
dnsHeader : PacketLang
dnsHeader = do ident <- bits 16 -- Request identifier
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
dnsRR = do domain <- dnsDomain
           ty <- bits 16
           check (validTYPE (val ty))
           cls <- bits 16
           check (validCLASS (val cls))
           ttl <- bits 32
           len <- bits 16 -- Length in octets of next field
           let vl = ((val len) * 8)
           prf <- check (vl > 0)
           bounded_bits vl prf

dns : PacketLang
dns = do header <- dnsHeader
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


