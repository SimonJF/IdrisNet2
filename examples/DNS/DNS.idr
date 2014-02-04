-- PacketLang implementation of DNS packets.
-- Also data representations of DNS packet.

-- PacketLang representations:
module DNS

import Network.PacketLang



-- Data representations of DNS Packets
{-
record DNSRequest : Type where
  MkDNSQ : (qdcount : Nat) ->
           (entries
-}

-- Verified implementation of the DNS packet specification

-- FIXME: Shift this to Network.PacketLang
value : Bounded n -> Int
value (BInt i p) = i

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
dnsRR : PacketLang
dnsRR = do domain <- list (dnsReference // dnsLabel)
           nullterm 
           ty <- bits 16
           check (validTYPE (value ty))
           cls <- bits 16
           check (validCLASS (value cls))
           ttl <- bits 32
           len <- bits 16 -- Length in octets of next field
           bits 32 -- FIXME: Temp
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


