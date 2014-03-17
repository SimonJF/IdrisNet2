module DNSCodes
import Network.Socket
import Network.PacketLang

%access public

{- A module specifying the relations between codes and higher-level
 - data types. There is nothing of remotely any interest in here. 

-}
-- %default total

data DNSHdrOpcode = QUERY | IQUERY | STATUS

instance Show DNSHdrOpcode where
  show QUERY = "QUERY"
  show IQUERY = "IQUERY"
  show STATUS = "STATUS"
  

{-
data DNSHdrOpcodeRel : Int -> DNSHdrOpcode -> Type where
  DNSHdrOpcodeRelQUERY : DNSHdrOpcodeRel 0 QUERY
  DNSHdrOpcodeRelIQUERY : DNSHdrOpcodeRel 1 IQUERY
  DNSHdrOpcodeRelSTATUS : DNSHdrOpcodeRel 2 STATUS


dnsOpcodeRel : (code : Int) -> (ty : DNSHdrOpcode) -> Maybe (DNSHdrOpcodeRel code ty)
dnsOpcodeRel 0 QUERY = Just DNSHdrOpcodeRelQUERY 
dnsOpcodeRel 1 IQUERY = Just DNSHdrOpcodeRelIQUERY
dnsOpcodeRel 2 STATUS = Just DNSHdrOpcodeRelSTATUS 
dnsOpcodeRel _ _ = Nothing
-}

codeToDNSOpcode : (code : Int) -> Maybe DNSHdrOpcode
codeToDNSOpcode 0 = Just QUERY
codeToDNSOpcode 1 = Just IQUERY
codeToDNSOpcode 2 = Just STATUS
codeToDNSOpcode _ = Nothing

dnsOpcodeToCode : DNSHdrOpcode -> Int
dnsOpcodeToCode QUERY = 0
dnsOpcodeToCode IQUERY = 1
dnsOpcodeToCode STATUS = 2

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

dnsCodeToQClass : (code : Int) -> Maybe DNSQClass
dnsCodeToQClass 1 = Just DNSQClassIN
dnsCodeToQClass 2 = Just DNSQClassCS
dnsCodeToQClass 3 = Just DNSQClassCH
dnsCodeToQClass 4 = Just DNSQClassHS
dnsCodeToQClass 255 = Just DNSQClassANY
dnsCodeToQClass _ = Nothing


dnsQClassToCode : DNSQClass -> Int
dnsQClassToCode DNSQClassIN = 1
dnsQClassToCode DNSQClassCS = 2
dnsQClassToCode DNSQClassCH = 3
dnsQClassToCode DNSQClassHS = 4
dnsQClassToCode DNSQClassANY = 255

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

 
dnsTypeRel : (code : Int) -> (ty : DNSType) -> Maybe (DNSTypeRel code ty)
dnsTypeRel 1 DNSTypeA = Just DNSTypeRelA
dnsTypeRel 2 DNSTypeNS = Just DNSTypeRelNS
dnsTypeRel 5 DNSTypeCNAME = Just DNSTypeRelCNAME
dnsTypeRel 10 DNSTypeNULL = Just DNSTypeRelNULL
dnsTypeRel 12 DNSTypePTR = Just DNSTypeRelPTR
dnsTypeRel 15 DNSTypeMX = Just DNSTypeRelMX
dnsTypeRel 16 DNSTypeTXT = Just DNSTypeRelTXT
dnsTypeRel 28 DNSTypeAAAA = Just DNSTypeRelAAAA

--dnsTypeToRel {code = 3} DNSTypeMD = DNSTypeRelMD
--dnsTypeToRel {code = 4} DNSTypeMF = DNSTypeRelMF
--dnsTypeToRel {code = 6} DNSTypeSOA = DNSTypeRelSOA
--dnsTypeToRel {code = 7} DNSTypeMB = DNSTypeRelMB
--dnsTypeToRel {code = 8} DNSTypeMG = DNSTypeRelMG
--dnsTypeToRel {code = 9} DNSTypeMR = DNSTypeRelMR
--dnsTypeToRel {code = 11} DNSTypeWKS = DNSTypeRelWKS
--dnsTypeToRel {code = 13} DNSTypeHINFO = DNSTypeRelHINFO
--dnsTypeToRel {code = 14} DNSTypeMINFO = DNSTypeRelMINFO

dnsCodeToQType : (code : Int) -> Maybe DNSQType
dnsCodeToQType 1 = Just DNSQTypeA
dnsCodeToQType 2 = Just DNSQTypeNS
dnsCodeToQType 5 = Just DNSQTypeCNAME
dnsCodeToQType 10 = Just DNSQTypeNULL
dnsCodeToQType 12 = Just DNSQTypePTR
dnsCodeToQType 15 = Just DNSQTypeMX
dnsCodeToQType 16 = Just DNSQTypeTXT
dnsCodeToQType 28 = Just DNSQTypeAAAA
dnsCodeToQType 252 = Just DNSQTypeAXFR
dnsCodeToQType 253 = Just DNSQTypeMAILB
dnsCodeToQType 254 = Just DNSQTypeMAILA
dnsCodeToQType 255 = Just DNSQTypeALL
dnsCodeToQType _ = Nothing


dnsQTypeToCode : DNSQType -> Int
dnsQTypeToCode DNSQTypeA = 1
dnsQTypeToCode DNSQTypeNS = 2
dnsQTypeToCode DNSQTypeCNAME = 5
dnsQTypeToCode DNSQTypeNULL = 10
dnsQTypeToCode DNSQTypePTR = 12
dnsQTypeToCode DNSQTypeMX = 15
dnsQTypeToCode DNSQTypeTXT = 16
dnsQTypeToCode DNSQTypeAAAA = 28
dnsQTypeToCode DNSQTypeAXFR = 252
dnsQTypeToCode DNSQTypeMAILB = 253
dnsQTypeToCode DNSQTypeMAILA = 254
dnsQTypeToCode DNSQTypeALL = 255

dnsQTypeRel : (code : Int) -> (ty : DNSQType) -> Maybe (DNSQTypeRel code ty)
dnsQTypeRel 1 DNSQTypeA = Just DNSQTypeRelA
dnsQTypeRel 2 DNSQTypeNS = Just DNSQTypeRelNS
dnsQTypeRel 5 DNSQTypeCNAME = Just DNSQTypeRelCNAME
dnsQTypeRel 10 DNSQTypeNULL = Just DNSQTypeRelNULL
dnsQTypeRel 12 DNSQTypePTR = Just DNSQTypeRelPTR
dnsQTypeRel 15 DNSQTypeMX = Just DNSQTypeRelMX
dnsQTypeRel 16 DNSQTypeTXT = Just DNSQTypeRelTXT
dnsQTypeRel 28 DNSQTypeAAAA = Just DNSQTypeRelAAAA
dnsQTypeRel 252 DNSQTypeAXFR = Just DNSQTypeRelAXFR
dnsQTypeRel 253 DNSQTypeMAILB = Just DNSQTypeRelMAILB
dnsQTypeRel 254 DNSQTypeMAILA = Just DNSQTypeRelMAILA
dnsQTypeRel 255 DNSQTypeALL = Just DNSQTypeRelALL
--dnsCodeToQType {ty = DNSQTypeMD} 3 = Just DNSQTypeRelMD
--dnsCodeToQType {ty = DNSQTypeMF} 4 = Just DNSQTypeRelMF
--dnsCodeToQType {ty = DNSQTypeSOA} 6 = Just DNSQTypeRelSOA
--dnsCodeToQType {ty = DNSQTypeMB} 7 = Just DNSQTypeRelMB
--dnsCodeToQType {ty = DNSQTypeMG} 8 = Just DNSQTypeRelMG
--dnsCodeToQType {ty = DNSQTypeMR} 9 = Just DNSQTypeRelMR
--dnsCodeToQType {ty = DNSQTypeWKS} 11 = Just DNSQTypeRelWKS
--dnsCodeToQType {ty = DNSQTypeHINFO} 13 = Just DNSQTypeRelHINFO
--dnsCodeToQType {ty = DNSQTypeMINFO} 14 = Just DNSQTypeRelMINFO

--dnsQTypeToRel {code = 3} DNSQTypeMD = DNSQTypeRelMD
--dnsQTypeToRel {code = 4} DNSQTypeMF = DNSQTypeRelMF
--dnsQTypeToRel {code = 6} DNSQTypeSOA = DNSQTypeRelSOA
--dnsQTypeToRel {code = 7} DNSQTypeMB = DNSQTypeRelMB
--dnsQTypeToRel {code = 8} DNSQTypeMG = DNSQTypeRelMG
--dnsQTypeToRel {code = 9} DNSQTypeMR = DNSQTypeRelMR
--dnsQTypeToRel {code = 11} DNSQTypeWKS = DNSQTypeRelWKS
--dnsQTypeToRel {code = 13} DNSQTypeHINFO = DNSQTypeRelHINFO
--dnsQTypeToRel {code = 14} DNSQTypeMINFO = DNSQTypeRelMINFO

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

dnsCodeToResponse : (code : Int) -> Maybe DNSResponse
dnsCodeToResponse 0 = Just DNSResponseNoError
dnsCodeToResponse 1 = Just DNSResponseFormatError
dnsCodeToResponse 2 = Just DNSResponseServerError
dnsCodeToResponse 3 = Just DNSResponseNameError
dnsCodeToResponse 4 = Just DNSResponseNotImplementedError
dnsCodeToResponse 5 = Just DNSResponseRefusedError
dnsCodeToResponse _ = Nothing


dnsResponseToCode : DNSResponse -> Int
dnsResponseToCode DNSResponseNoError = 0
dnsResponseToCode DNSResponseFormatError = 1
dnsResponseToCode DNSResponseServerError = 2
dnsResponseToCode DNSResponseNameError = 3
dnsResponseToCode DNSResponseNotImplementedError = 4


DomainFragment : Type
DomainFragment = String

data DNSPayloadType = DNSIPv4 | DNSIPv6 | DNSDomain | DNSUnimplementedPayload

data DNSPayload : DNSPayloadType -> Type where
  -- TODO: It would be nice to specialise SocketAddress further, ideally
  -- by declaring it as a GADT and parameterising it over the type of 
  -- address. Not quite sure how that's going to work with the rest of
  -- the code though...
  DNSIPv4Payload : SocketAddress -> DNSPayload DNSIPv4 
  DNSIPv6Payload : SocketAddress -> DNSPayload DNSIPv6
  DNSDomainPayload : List DomainFragment -> DNSPayload DNSDomain
  DNSNotImplementedPayload : DNSPayload DNSUnimplementedPayload -- get out clause!


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


getPayloadRel : (pl_ty : DNSPayloadType) ->
              (ty : DNSType) -> 
              (cls : DNSClass) -> 
              Maybe (DNSPayloadRel ty cls pl_ty)
getPayloadRel DNSIPv4 DNSTypeA DNSClassIN = Just DNSPayloadRelIP
getPayloadRel DNSIPv6 DNSTypeAAAA DNSClassIN = Just DNSPayloadRelIP6
getPayloadRel DNSDomain DNSTypeCNAME DNSClassIN = Just DNSPayloadRelCNAME
getPayloadRel DNSDomain DNSTypeNS DNSClassIN = Just DNSPayloadRelNS
getPayloadRel _ _ _ = Nothing


payloadType : DNSType -> DNSClass -> Maybe DNSPayloadType
payloadType DNSTypeA DNSClassIN = Just DNSIPv4
payloadType DNSTypeAAAA DNSClassIN = Just DNSIPv6
payloadType DNSTypeNS DNSClassIN =  Just DNSDomain
payloadType DNSTypeCNAME DNSClassIN = Just DNSDomain
payloadType _ _ = Nothing

{-
getPayloadRel' : (ty_rel : DNSTypeRel ty_code ty) ->
                 (cls_rel : DNSClassRel cls_code cls) ->
                 Maybe (DNSPayloadRel ty cls pl_ty)
getPayloadRel' {pl_ty = DNSIPv4} DNSTypeRelA DNSClassRelIN = Just DNSPayloadRelIP
getPayloadRel' {pl_ty = DNSDomain} DNSTypeRelNS DNSClassRelIN = Just DNSPayloadRelNS 
getPayloadRel' {pl_ty = DNSDomain} DNSTypeRelCNAME DNSClassRelIN = Just DNSPayloadRelCNAME
getPayloadRel' {pl_ty = DNSIPv6} DNSTypeRelAAAA DNSClassRelIN = Just DNSPayloadRelIP6
getPayloadRel' _ _ = Nothing
-}
{-
getPayloadRel' DNSTypeRelNULL cls_rel = ?mv_4
getPayloadRel' DNSTypeRelPTR cls_rel = ?mv_5
getPayloadRel' DNSTypeRelMX cls_rel = ?mv_6
getPayloadRel' DNSTypeRelTXT cls_rel = ?mv_7
-}
{-
payloadType : DNSType -> DNSClass -> Type
payloadType A IN = DNSPayload DNSIPv4
payloadType AAAA IN = DNSPayload DNSIPv6
payloadType NS IN = DNSPayload DNSDomain
payloadType CNAME IN = DNSPayload DNSDomain
payloadType _ _ = DNSPayload DNSUnimplementedPayload
-}

-- payloadType q


