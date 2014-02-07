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
codeToDNSQType 252 = Just MAILB
codeToDNSQType 252 = Just MAILA
codeToDNSQType 252 = Just ALL
codeToDNSQType i = map DNSBaseTy (codeToDNSType i)

instance Code DNSType where
  toCode x = dnsTypeToCode x
  fromCode i = codeToDNSType i

instance Code DNSQType where
  toCode x = dnsQTypeToCode x
  fromCode i = codeToDNSQType i

data DNSQuestion : Type where
  MkDNSQuestion : DNSQuestion

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

data DNSHeader : Type where
  MkDNSHeader : (dnsHdrId : Int) ->
                (dnsHdrIsQuery : Bool) ->
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
          (dnsPcktAdditionals : Vect dnsPcktNSCount DNSRecord) ->
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

nullterm : PacketLang
nullterm = do nt <- bits 8
              check ((value nt) == 0)

-- DNS allows compression in the form of references.
-- These take the form of two octets, the first two bits of which 
-- are 11. The rest is 14 bits.
dnsReference : PacketLang
dnsReference = do tag1 <- bits 1
                  tag2 <- bits 1
                  let v1 = value tag1
                  let v2 = value tag2
                  prop_eq v1 1
                  prop_eq v2 1
                  bits 14

 --                 check (((value tag1) == 1) && ((value tag2) == 1))


dnsLabel : PacketLang
dnsLabel = do len <- bits 8
              check ((value len) /= 0) 
              listn (intToNat $ value len) (bits 8)

dnsQuestion : PacketLang
dnsQuestion = do list (dnsReference // dnsLabel)
                 nullterm -- Zero-length octet for the root domain
                 qtype <- bits 16
                 check (validQTYPE (value qtype))
                 qclass <- bits 16 
                 check (validQCLASS 16)

dnsHeader : PacketLang
dnsHeader = do ident <- bits 16 -- Request identifier
               qr <- bool -- Query or response 
               opcode <- bits 4 -- Which type of query? Only 0, 1 and 2 valid
               aa <- bool -- Only set in response, is responding server authority?
               tc <- bool -- Was message truncated?
               rd <- bool -- Recursion desired, set in query, copied into response
               ra <- bool -- Recursion available; is support available in NS?
               z  <- bool -- Must be 0.
               bits 4 -- Response code, only 0-5 valid

-- DNS Resource Record
-- The same for answers, authorities and additional info.

{-
RDLENGTH        an unsigned 16 bit integer that specifies the length in
                octets of the RDATA field.

RDATA           a variable length string of octets that describes the
                resource.  The format of this information varies
                according to the TYPE and CLASS of the resource record.
                For example, the if the TYPE is A and the CLASS is IN,
                the RDATA field is a 4 octet ARPA Internet address.
-}


dnsRR : PacketLang
dnsRR = do domain <- list (dnsReference // dnsLabel)
           nullterm 
           ty <- bits 16
           check (validTYPE (value ty))
           cls <- bits 16
           check (validCLASS (value cls))
           ttl <- bits 32
           len <- bits 16 -- Length in octets of next field
           let vl = ((value len) * 8)
           prf <- check (vl > 0)
           bounded_bits vl prf

--bits 32 -- FIXME: Temp
            -- 32 -- FIXME: Temp. Need proper verification here...
           --let vl = value len
           --prf <- check (vl > 0)
           --CHUNK (Bit vl prf)
           --bits ((value len) * 8) prf -- Data payload. This is a tad more complex, TODO: more verification
                             -- although RFC is pretty unspecific on this, 
                             -- and it's generally just 4-byte IP

dns : PacketLang
dns = do header <- dnsHeader
         -- Technically these are parts of the header, but we 
         -- need them in scope for later
         qdcount <- bits 16 -- Number of entries in question section
         ancount <- bits 16 -- Number of entries in the answer section
         nscount <- bits 16 -- Number of NS resource records in auth records
         arcount <- bits 16 -- Number of resource records in additional records section
         question <- dnsQuestion 
         answer <- dnsRR
         authority <- dnsRR
         dnsRR -- Additional


