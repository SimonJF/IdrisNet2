module DNSCodes
import Network.Socket
import Network.PacketLang

%access public

{- A module specifying the relations between codes and higher-level
 - data types. There is nothing of remotely any interest in here. 

-}
%default total

data DNSHdrOpcode = QUERY | IQUERY | STATUS

instance Show DNSHdrOpcode where
  show QUERY = "QUERY"
  show IQUERY = "IQUERY"
  show STATUS = "STATUS"

dnsCodeToOpcode : Bounded 4 -> Maybe DNSHdrOpcode
dnsCodeToOpcode (BInt 0 oh) = Just QUERY
dnsCodeToOpcode (BInt 1 oh) = Just IQUERY
dnsCodeToOpcode (BInt 2 oh) = Just STATUS
dnsCodeToOpcode _ = Nothing

dnsOpcodeToCode : DNSHdrOpcode -> Bounded 4
dnsOpcodeToCode QUERY = BInt 0 oh
dnsOpcodeToCode IQUERY = BInt 1 oh
dnsOpcodeToCode STATUS = BInt 2 oh

data DNSClass = DNSClassIN -- Internet
  -- None of the following are used, anyway...
{-
              | DNSClassCS  -- CSNET
              | DNSClassCH -- CHAOS
              | DNSClassHS -- Hesiod
-}

instance Show DNSClass where
  show DNSClassIN = "IN"

data DNSClassRel : Int -> DNSClass -> Type where
  DNSClassRelIN : DNSClassRel 1 DNSClassIN
{-
  DNSClassRelCS : DNSClassRel 2 DNSClassCS
  DNSClassRelCH : DNSClassRel 3 DNSClassCH
  DNSClassRelHS : DNSClassRel 4 DNSClassHS
-}
dnsClassRel : (code : Int) -> (cls : DNSClass) -> Maybe (DNSClassRel code cls)
dnsClassRel 1 DNSClassIN = Just DNSClassRelIN
dnsClassRel _ _ = Nothing 
{-
dnsClassToRel {code = 2} DNSClassCS = DNSClassRelCS
dnsClassToRel {code = 3} DNSClassCH = DNSClassRelCH
dnsClassToRel {code = 4} DNSClassHS = DNSClassRelHS
-}
dnsCodeToClass : (code : Int) -> Maybe DNSClass
dnsCodeToClass 1 = Just DNSClassIN
dnsCodeToClass _ = Nothing

dnsClassToCode : DNSClass -> Int
dnsClassToCode DNSClassIN = 1

dnsCodeToClass' : Bounded 16 -> Maybe DNSClass
dnsCodeToClass' (BInt 1 oh) = Just DNSClassIN
dnsCodeToClass' _ = Nothing

dnsClassToCode' : DNSClass -> Bounded 16
dnsClassToCode' DNSClassIN = BInt 1 oh

{-
dnsCodeToClass {cls = DNSClassCS} 2 = Just DNSClassRelCS
dnsCodeToClass {cls = DNSClassCH} 3 = Just DNSClassRelCH
dnsCodeToClass {cls = DNSClassHS} 4 = Just DNSClassRelHS
-}


data DNSQClass = DNSQClassIN -- Internet
               | DNSQClassANY -- Any class
               | DNSQClassCS  -- CSNET
               | DNSQClassCH -- CHAOS
               | DNSQClassHS -- Hesiod

instance Show DNSQClass where
  show DNSQClassIN = "IN"
  show DNSQClassANY = "ANY"
  show DNSQClassCS = "CS"
  show DNSQClassCH = "CH"
  show DNSQClassHS = "HS"

dnsCodeToQClass : (Bounded 16) -> Maybe DNSQClass
dnsCodeToQClass (BInt 1 oh) = Just DNSQClassIN
dnsCodeToQClass (BInt 2 oh) = Just DNSQClassCS
dnsCodeToQClass (BInt 3 oh) = Just DNSQClassCH
dnsCodeToQClass (BInt 4 oh) = Just DNSQClassHS
dnsCodeToQClass (BInt 255 oh) = Just DNSQClassANY
dnsCodeToQClass _ = Nothing


dnsQClassToCode : DNSQClass -> Bounded 16
dnsQClassToCode DNSQClassIN = BInt 1 oh
dnsQClassToCode DNSQClassCS = BInt 2 oh
dnsQClassToCode DNSQClassCH = BInt 3 oh
dnsQClassToCode DNSQClassHS = BInt 4 oh
dnsQClassToCode DNSQClassANY = BInt 255 oh

-- A few constants... Might change this
A_VAL : Int
A_VAL = 1

NS_VAL : Int
NS_VAL = 2

CNAME_VAL : Int
CNAME_VAL = 5

AAAA_VAL : Int
AAAA_VAL = 28

IN_VAL : Int
IN_VAL = 1

-- Type descriptions taken from RFC1035
           -- s | DNSTypeMD -- A mail destination (obsolete, use MX)
           -- s | DNSTypeMF -- A mail forwarder (obsolete, use MX)
           -- s | DNSTypeSOA -- Start of authority
           -- s | DNSTypeMB -- Mailbox domain name
           -- s | DNSTypeMG -- Mail group member
           -- s | DNSTypeMR -- Mail rename domain name
           -- s | DNSTypeWKS -- Well-known service description
           -- s | DNSTypeHINFO -- Host information
           -- s | DNSTypeMINFO -- Mailbox or mail list informatio n

data DNSType = DNSTypeA -- A host address
             | DNSTypeNS -- An authoritative name server
             | DNSTypeCNAME -- Canonical name for an alias
             | DNSTypeNULL -- Null RR
             | DNSTypePTR -- A domain name pointer
             | DNSTypeMX -- Mail exchange
             | DNSTypeTXT -- Text record
             | DNSTypeAAAA -- IPv6 Host Address


instance Show DNSType where
  show DNSTypeA = "A"
  show DNSTypeNS = "NA"
  show DNSTypeCNAME = "CNAME"
  show DNSTypeNULL = "NULL"
  show DNSTypePTR = "MX"
  show DNSTypeTXT = "TXT"
  show DNSTypeAAAA = "AAAA"


-- Question types
data DNSQType = DNSQTypeAXFR -- Request for transfer of entire zone
              | DNSQTypeMAILB -- Request for mailbox-related records
              | DNSQTypeMAILA -- Request for mail agent RRs (obsolete, use MX) 
              | DNSQTypeALL -- Request for all records
              | DNSQTypeA -- A host address
              | DNSQTypeNS -- An authoritative name server
              | DNSQTypeCNAME -- Canonical name for an alias
              | DNSQTypeNULL -- Null RR
              | DNSQTypePTR -- A domain name pointer
              | DNSQTypeMX -- Mail exchange
              | DNSQTypeTXT -- Text record
              | DNSQTypeAAAA -- IPv6 Host Address


instance Show DNSQType where
  show DNSQTypeAXFR = "AXFR"
  show DNSQTypeMAILA = "MAILA"
  show DNSQTypeMAILB = "MAILB"
  show DNSQTypeALL = "ALL"
  show DNSQTypeA = "A"
  show DNSQTypeNS = "NA"
  show DNSQTypeCNAME = "CNAME"
  show DNSQTypeNULL = "NULL"
  show DNSQTypePTR = "MX"
  show DNSQTypeTXT = "TXT"
  show DNSQTypeAAAA = "AAAA"

-- stop parsing comments 
            -- s | DNSQTypeMD -- A mail destination (obsolete, use MX)
            -- s | DNSQTypeMF -- A mail forwarder (obsolete, use MX)
            -- s | DNSQTypeSOA -- Start of authority
            -- s | DNSQTypeMB -- Mailbox domain name
            -- s | DNSQTypeMG -- Mail group member
            -- s | DNSQTypeMR -- Mail rename domain name
            -- s | DNSQTypeWKS -- Well-known service description
            -- s | DNSQTypeHINFO -- Host information
            -- s | DNSQTypeMINFO -- Mailbox or mail list informatio n


{- TODO (possibly?): make this a tad cleaner -}

data DNSTypeRel : Int -> DNSType -> Type where
  DNSTypeRelA : DNSTypeRel 1 DNSTypeA 
  DNSTypeRelNS : DNSTypeRel NS_VAL DNSTypeNS
  DNSTypeRelCNAME : DNSTypeRel CNAME_VAL DNSTypeCNAME
  DNSTypeRelNULL : DNSTypeRel 10 DNSTypeNULL
  DNSTypeRelPTR : DNSTypeRel 12 DNSTypePTR
  DNSTypeRelMX : DNSTypeRel 15 DNSTypeMX
  DNSTypeRelTXT : DNSTypeRel 16 DNSTypeTXT
  DNSTypeRelAAAA : DNSTypeRel AAAA_VAL DNSTypeAAAA
 -- DNSTypeRelMD : DNSTypeRel 3 DNSTypeMD
 -- DNSTypeRelMF : DNSTypeRel 4 DNSTypeMF
 -- DNSTypeRelSOA : DNSTypeRel 6 DNSTypeSOA
 -- DNSTypeRelMB : DNSTypeRel 7 DNSTypeMB
 -- DNSTypeRelMG : DNSTypeRel 8 DNSTypeMG
 -- DNSTypeRelMR : DNSTypeRel 9 DNSTypeMR
  -- DNSTypeRelWKS : DNSTypeRel 11 DNSTypeWKS
  -- DNSTypeRelHINFO : DNSTypeRel 13 DNSTypeHINFO
  -- DNSTypeRelMINFO : DNSTypeRel 14 DNSTypeMINFO

data DNSQTypeRel : Int -> DNSQType -> Type where
  DNSQTypeRelA : DNSQTypeRel 1 DNSQTypeA 
  DNSQTypeRelNS : DNSQTypeRel NS_VAL DNSQTypeNS
  DNSQTypeRelCNAME : DNSQTypeRel CNAME_VAL DNSQTypeCNAME
  DNSQTypeRelNULL : DNSQTypeRel 10 DNSQTypeNULL
  DNSQTypeRelPTR : DNSQTypeRel 12 DNSQTypePTR
  DNSQTypeRelMX : DNSQTypeRel 15 DNSQTypeMX
  DNSQTypeRelTXT : DNSQTypeRel 16 DNSQTypeTXT
  DNSQTypeRelAAAA : DNSQTypeRel AAAA_VAL DNSQTypeAAAA
  DNSQTypeRelAXFR : DNSQTypeRel 252 DNSQTypeAXFR
  DNSQTypeRelMAILB : DNSQTypeRel 253 DNSQTypeMAILB
  DNSQTypeRelMAILA : DNSQTypeRel 254 DNSQTypeMAILA
  DNSQTypeRelALL : DNSQTypeRel 255 DNSQTypeALL
--  DNSQTypeRelMD : DNSQTypeRel 3 DNSQTypeMD
--  DNSQTypeRelMF : DNSQTypeRel 4 DNSQTypeMF
--  DNSQTypeRelSOA : DNSQTypeRel 6 DNSQTypeSOA
--  DNSQTypeRelMB : DNSQTypeRel 7 DNSQTypeMB
--  DNSQTypeRelMG : DNSQTypeRel 8 DNSQTypeMG
--  DNSQTypeRelMR : DNSQTypeRel 9 DNSQTypeMR
--  DNSQTypeRelWKS : DNSQTypeRel 11 DNSQTypeWKS
--  DNSQTypeRelHINFO : DNSQTypeRel 13 DNSQTypeHINFO
--  DNSQTypeRelMINFO : DNSQTypeRel 14 DNSQTypeMINFO

dnsTypeToCode' : DNSType -> Bounded 16
dnsTypeToCode' DNSTypeA = (BInt 1 oh)
dnsTypeToCode' DNSTypeNS = (BInt 2 oh) 
dnsTypeToCode' DNSTypeCNAME = (BInt 5 oh)
dnsTypeToCode' DNSTypeNULL = (BInt 10 oh)
dnsTypeToCode' DNSTypePTR = (BInt 12 oh)
dnsTypeToCode' DNSTypeMX = (BInt 15 oh)
dnsTypeToCode' DNSTypeTXT = (BInt 16 oh)
dnsTypeToCode' DNSTypeAAAA = (BInt 28 oh)

dnsTypeToCode : DNSType -> Int
dnsTypeToCode DNSTypeA = 1
dnsTypeToCode DNSTypeNS = 2
dnsTypeToCode DNSTypeCNAME = 5
dnsTypeToCode DNSTypeNULL = 10
dnsTypeToCode DNSTypePTR = 12
dnsTypeToCode DNSTypeMX = 15
dnsTypeToCode DNSTypeTXT = 16
dnsTypeToCode DNSTypeAAAA = 28


dnsCodeToType' : (Bounded 16) -> Maybe DNSType
dnsCodeToType' (BInt 1 oh) = Just DNSTypeA
dnsCodeToType' (BInt 2 oh) = Just DNSTypeNS
dnsCodeToType' (BInt 5 oh) = Just DNSTypeCNAME
dnsCodeToType' (BInt 10 oh) = Just DNSTypeNULL
dnsCodeToType' (BInt 12 oh) = Just DNSTypePTR
dnsCodeToType' (BInt 15 oh) = Just DNSTypeMX
dnsCodeToType' (BInt 16 oh) = Just DNSTypeTXT
dnsCodeToType' (BInt 28 oh) = Just DNSTypeAAAA
dnsCodeToType' (BInt _ oh) = Nothing


dnsCodeToType : (code : Int) -> Maybe DNSType
dnsCodeToType 1 = Just DNSTypeA
dnsCodeToType 2 = Just DNSTypeNS
dnsCodeToType 5 = Just DNSTypeCNAME
dnsCodeToType 10 = Just DNSTypeNULL
dnsCodeToType 12 = Just DNSTypePTR
dnsCodeToType 15 = Just DNSTypeMX
dnsCodeToType 16 = Just DNSTypeTXT
dnsCodeToType 28 = Just DNSTypeAAAA
dnsCodeToType _ = Nothing

--dnsCodeToType {ty = DNSTypeMD} 3 = Just DNSTypeRelMD
--dnsCodeToType {ty = DNSTypeMF} 4 = Just DNSTypeRelMF
--dnsCodeToType {ty = DNSTypeSOA} 6 = Just DNSTypeRelSOA
--dnsCodeToType {ty = DNSTypeMB} 7 = Just DNSTypeRelMB
--dnsCodeToType {ty = DNSTypeMG} 8 = Just DNSTypeRelMG
--dnsCodeToType {ty = DNSTypeMR} 9 = Just DNSTypeRelMR
--dnsCodeToType {ty = DNSTypeWKS} 11 = Just DNSTypeRelWKS
--dnsCodeToType {ty = DNSTypeHINFO} 13 = Just DNSTypeRelHINFO
--dnsCodeToType {ty = DNSTypeMINFO} 14 = Just DNSTypeRelMINFO

dnsCodeToQType : Bounded 16 -> Maybe DNSQType
dnsCodeToQType (BInt 1 oh) = Just DNSQTypeA
dnsCodeToQType (BInt 2 oh) = Just DNSQTypeNS
dnsCodeToQType (BInt 5 oh) = Just DNSQTypeCNAME
dnsCodeToQType (BInt 10 oh) = Just DNSQTypeNULL
dnsCodeToQType (BInt 12 oh) = Just DNSQTypePTR
dnsCodeToQType (BInt 15 oh) = Just DNSQTypeMX
dnsCodeToQType (BInt 16 oh) = Just DNSQTypeTXT
dnsCodeToQType (BInt 28 oh) = Just DNSQTypeAAAA
dnsCodeToQType (BInt 252 oh) = Just DNSQTypeAXFR
dnsCodeToQType (BInt 253 oh) = Just DNSQTypeMAILB
dnsCodeToQType (BInt 254 oh) = Just DNSQTypeMAILA
dnsCodeToQType (BInt 255 oh) = Just DNSQTypeALL
dnsCodeToQType _ = Nothing


dnsQTypeToCode : DNSQType -> Bounded 16
dnsQTypeToCode DNSQTypeA = (BInt 1 oh)
dnsQTypeToCode DNSQTypeNS = (BInt 2 oh)
dnsQTypeToCode DNSQTypeCNAME = (BInt 5 oh)
dnsQTypeToCode DNSQTypeNULL = (BInt 10 oh)
dnsQTypeToCode DNSQTypePTR = (BInt 12 oh)
dnsQTypeToCode DNSQTypeMX = (BInt 15 oh)
dnsQTypeToCode DNSQTypeTXT = (BInt 16 oh)
dnsQTypeToCode DNSQTypeAAAA = (BInt 28 oh)
dnsQTypeToCode DNSQTypeAXFR = (BInt 252 oh)
dnsQTypeToCode DNSQTypeMAILB = (BInt 253 oh)
dnsQTypeToCode DNSQTypeMAILA = (BInt 254 oh)
dnsQTypeToCode DNSQTypeALL = (BInt 255 oh)

data DNSResponse = DNSResponseNoError
                 | DNSResponseFormatError
                 | DNSResponseServerError
                 | DNSResponseNameError
                 | DNSResponseNotImplementedError
                 | DNSResponseRefusedError


instance Show DNSResponse where
  show DNSResponseNoError = "No error"
  show DNSResponseFormatError = "Format error"
  show DNSResponseServerError = "Server error"
  show DNSResponseNameError = "Name error"
  show DNSResponseNotImplementedError = "Not implemented error"
  show DNSResponseRefusedError = "Refused error"

dnsCodeToResponse : Bounded 4 -> Maybe DNSResponse
dnsCodeToResponse (BInt 0 oh) = Just DNSResponseNoError
dnsCodeToResponse (BInt 1 oh) = Just DNSResponseFormatError
dnsCodeToResponse (BInt 2 oh) = Just DNSResponseServerError
dnsCodeToResponse (BInt 3 oh) = Just DNSResponseNameError
dnsCodeToResponse (BInt 4 oh) = Just DNSResponseNotImplementedError
dnsCodeToResponse (BInt 5 oh) = Just DNSResponseRefusedError
dnsCodeToResponse _ = Nothing


dnsResponseToCode : DNSResponse -> Bounded 4
dnsResponseToCode DNSResponseNoError = BInt 0 oh
dnsResponseToCode DNSResponseFormatError = BInt 1 oh
dnsResponseToCode DNSResponseServerError = BInt 2 oh
dnsResponseToCode DNSResponseNameError = BInt 3 oh
dnsResponseToCode DNSResponseNotImplementedError = BInt 4 oh
dnsResponseToCode DNSResponseRefusedError = BInt 5 oh


DomainFragment : Type
DomainFragment = String

data DNSPayloadType = DNSIPv4 | DNSIPv6 | DNSDomain 

data DNSPayload : DNSPayloadType -> Type where
  DNSIPv4Payload : SocketAddress -> DNSPayload DNSIPv4 
  DNSIPv6Payload : SocketAddress -> DNSPayload DNSIPv6
  DNSDomainPayload : List DomainFragment -> DNSPayload DNSDomain

data DNSPayloadRel : DNSType -> DNSClass -> DNSPayloadType -> Type where
  DNSPayloadRelIP : DNSPayloadRel DNSTypeA DNSClassIN DNSIPv4
  DNSPayloadRelIP6 : DNSPayloadRel DNSTypeAAAA DNSClassIN DNSIPv6
  DNSPayloadRelCNAME : DNSPayloadRel DNSTypeCNAME DNSClassIN DNSDomain
  DNSPayloadRelNS : DNSPayloadRel DNSTypeNS DNSClassIN DNSDomain


showPayload : (DNSPayloadRel rrt rrc pl_ty) -> DNSPayload pl_ty -> String
showPayload DNSPayloadRelIP (DNSIPv4Payload addr) = "IPv4: " ++ (show addr)
showPayload DNSPayloadRelIP6 (DNSIPv6Payload addr) = "IPv6 " ++ (show addr)
showPayload DNSPayloadRelCNAME (DNSDomainPayload dom) = "Domain: " ++ show dom
showPayload DNSPayloadRelNS (DNSDomainPayload dom) = "Domain: " ++ show dom

