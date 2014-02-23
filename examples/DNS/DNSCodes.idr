module DNSCodes
import Network.Socket

%access public

{- A module specifying the relations between codes and higher-level
 - data types. There is nothing of remotely any interest in here. 

- TODO: As nice as it would be, the whole "make me a relation with an implicit" thing 
- doesn't work. Firstly, we'll have to decode to get a DNSType / DNSClass, *then*
- match, using both the code and type, to get the relation.

- Makes the code a tad uglier, but *should* work, Lord Helix permitting.

-}
-- %default total

data DNSHdrOpcode = QUERY | IQUERY | STATUS

data DNSHdrOpcodeRel : Int -> DNSHdrOpcode -> Type where
  DNSHdrOpcodeRelQUERY : DNSHdrOpcodeRel 0 QUERY
  DNSHdrOpcodeRelIQUERY : DNSHdrOpcodeRel 1 IQUERY
  DNSHdrOpcodeRelSTATUS : DNSHdrOpcodeRel 2 STATUS


dnsOpcodeToCode : {code : Int} -> (ty : DNSHdrOpcode) -> DNSHdrOpcodeRel code ty
dnsOpcodeToCode {code = 0} QUERY = DNSHdrOpcodeRelQUERY 
dnsOpcodeToCode {code = 1} IQUERY = DNSHdrOpcodeRelIQUERY
dnsOpcodeToCode {code = 2} STATUS = DNSHdrOpcodeRelSTATUS 

codeToDNSOpcode : {ty : DNSHdrOpcode} -> (code : Int) -> Maybe (DNSHdrOpcodeRel code ty)
codeToDNSOpcode {ty = QUERY} 0 = Just DNSHdrOpcodeRelQUERY
codeToDNSOpcode {ty = IQUERY} 1 = Just DNSHdrOpcodeRelIQUERY
codeToDNSOpcode {ty = STATUS} 2 = Just DNSHdrOpcodeRelSTATUS 

data DNSClass = DNSClassIN -- Internet
  -- None of the following are used, anyway...
{-
              | DNSClassCS  -- CSNET
              | DNSClassCH -- CHAOS
              | DNSClassHS -- Hesiod
-}
data DNSClassRel : Int -> DNSClass -> Type where
  DNSClassRelIN : DNSClassRel 1 DNSClassIN
{-
  DNSClassRelCS : DNSClassRel 2 DNSClassCS
  DNSClassRelCH : DNSClassRel 3 DNSClassCH
  DNSClassRelHS : DNSClassRel 4 DNSClassHS
-}
dnsClassToCode : {code : Int} -> (cls : DNSClass) -> DNSClassRel code cls
dnsClassToCode {code = 1} DNSClassIN = DNSClassRelIN
{-
dnsClassToCode {code = 2} DNSClassCS = DNSClassRelCS
dnsClassToCode {code = 3} DNSClassCH = DNSClassRelCH
dnsClassToCode {code = 4} DNSClassHS = DNSClassRelHS
-}
dnsCodeToClass : {cls : DNSClass} -> (code : Int) -> Maybe (DNSClassRel code cls)
dnsCodeToClass {cls = DNSClassIN} 1 = Just DNSClassRelIN
dnsCodeToClass _ = Nothing
{-
dnsCodeToClass {cls = DNSClassCS} 2 = Just DNSClassRelCS
dnsCodeToClass {cls = DNSClassCH} 3 = Just DNSClassRelCH
dnsCodeToClass {cls = DNSClassHS} 4 = Just DNSClassRelHS
-}


data DNSQClass = DNSQClassIN -- Internet
               | DNSQClassANY -- Any class
  -- None of the following are used, anyway...
{-
              | DNSQClassCS  -- CSNET
              | DNSQClassCH -- CHAOS
              | DNSQClassHS -- Hesiod
-}
data DNSQClassRel : Int -> DNSQClass -> Type where
  DNSQClassRelIN : DNSQClassRel 1 DNSQClassIN
  DNSQClassRelANY : DNSQClassRel 255 DNSQClassANY
--  DNSQClassRelCS : DNSQClassRel 2 DNSQClassCS
--  DNSQClassRelCH : DNSQClassRel 3 DNSQClassCH
--  DNSQClassRelHS : DNSQClassRel 4 DNSQClassHS

dnsQClassToCode : {code : Int} -> (cls : DNSQClass) -> DNSQClassRel code cls
dnsQClassToCode {code = 1} DNSQClassIN = DNSQClassRelIN
dnsQClassToCode {code = 255} DNSQClassANY = DNSQClassRelANY
--dnsQClassToCode {code = 2} DNSQClassCS = DNSQClassRelCS
--dnsQClassToCode {code = 3} DNSQClassCH = DNSQClassRelCH
--dnsQClassToCode {code = 4} DNSQClassHS = DNSQClassRelHS

dnsCodeToQClass : {cls : DNSQClass} -> (code : Int) -> Maybe (DNSQClassRel code cls)
dnsCodeToQClass {cls = DNSQClassIN} 1 = Just DNSQClassRelIN
dnsCodeToQClass {cls = DNSQClassANY} 255 = Just DNSQClassRelANY
dnsCodeToQClass _ = Nothing
--dnsCodeToQClass {cls = DNSQClassCS} 2 = Just DNSQClassRelCS
--dnsCodeToQClass {cls = DNSQClassCH} 3 = Just DNSQClassRelCH
--dnsCodeToQClass {cls = DNSQClassHS} 4 = Just DNSQClassRelHS

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

dnsCodeToType : {ty : DNSType} -> (code : Int) -> Maybe (DNSTypeRel code ty)
dnsCodeToType {ty = DNSTypeA} 1 = Just DNSTypeRelA
dnsCodeToType {ty = DNSTypeNS} 2 = Just DNSTypeRelNS
dnsCodeToType {ty = DNSTypeCNAME} 5 = Just DNSTypeRelCNAME
dnsCodeToType {ty = DNSTypeNULL} 10 = Just DNSTypeRelNULL
dnsCodeToType {ty = DNSTypePTR} 12 = Just DNSTypeRelPTR
dnsCodeToType {ty = DNSTypeMX} 15 = Just DNSTypeRelMX
dnsCodeToType {ty = DNSTypeTXT} 16 = Just DNSTypeRelTXT
dnsCodeToType {ty = DNSTypeAAAA} 28 = Just DNSTypeRelAAAA
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

 
dnsTypeToCode : {code : Int} -> (ty : DNSType) -> DNSTypeRel code ty
dnsTypeToCode {code = 1} DNSTypeA = DNSTypeRelA
dnsTypeToCode {code = 2} DNSTypeNS = DNSTypeRelNS
dnsTypeToCode {code = 5} DNSTypeCNAME = DNSTypeRelCNAME
dnsTypeToCode {code = 10} DNSTypeNULL = DNSTypeRelNULL
dnsTypeToCode {code = 12} DNSTypePTR = DNSTypeRelPTR
dnsTypeToCode {code = 15} DNSTypeMX = DNSTypeRelMX
dnsTypeToCode {code = 16} DNSTypeTXT = DNSTypeRelTXT
dnsTypeToCode {code = 28} DNSTypeAAAA = DNSTypeRelAAAA
--dnsTypeToCode {code = 3} DNSTypeMD = DNSTypeRelMD
--dnsTypeToCode {code = 4} DNSTypeMF = DNSTypeRelMF
--dnsTypeToCode {code = 6} DNSTypeSOA = DNSTypeRelSOA
--dnsTypeToCode {code = 7} DNSTypeMB = DNSTypeRelMB
--dnsTypeToCode {code = 8} DNSTypeMG = DNSTypeRelMG
--dnsTypeToCode {code = 9} DNSTypeMR = DNSTypeRelMR
--dnsTypeToCode {code = 11} DNSTypeWKS = DNSTypeRelWKS
--dnsTypeToCode {code = 13} DNSTypeHINFO = DNSTypeRelHINFO
--dnsTypeToCode {code = 14} DNSTypeMINFO = DNSTypeRelMINFO

dnsCodeToQType : {ty : DNSQType} -> (code : Int) -> Maybe (DNSQTypeRel code ty)
dnsCodeToQType {ty = DNSQTypeA} 1 = Just DNSQTypeRelA
dnsCodeToQType {ty = DNSQTypeNS} 2 = Just DNSQTypeRelNS
dnsCodeToQType {ty = DNSQTypeCNAME} 5 = Just DNSQTypeRelCNAME
dnsCodeToQType {ty = DNSQTypeNULL} 10 = Just DNSQTypeRelNULL
dnsCodeToQType {ty = DNSQTypePTR} 12 = Just DNSQTypeRelPTR
dnsCodeToQType {ty = DNSQTypeMX} 15 = Just DNSQTypeRelMX
dnsCodeToQType {ty = DNSQTypeTXT} 16 = Just DNSQTypeRelTXT
dnsCodeToQType {ty = DNSQTypeAAAA} 28 = Just DNSQTypeRelAAAA
dnsCodeToQType {ty = DNSQTypeAXFR} 252 = Just DNSQTypeRelAXFR
dnsCodeToQType {ty = DNSQTypeMAILB} 253 = Just DNSQTypeRelMAILB
dnsCodeToQType {ty = DNSQTypeMAILA} 254 = Just DNSQTypeRelMAILA
dnsCodeToQType {ty = DNSQTypeALL} 255 = Just DNSQTypeRelALL
--dnsCodeToQType {ty = DNSQTypeMD} 3 = Just DNSQTypeRelMD
--dnsCodeToQType {ty = DNSQTypeMF} 4 = Just DNSQTypeRelMF
--dnsCodeToQType {ty = DNSQTypeSOA} 6 = Just DNSQTypeRelSOA
--dnsCodeToQType {ty = DNSQTypeMB} 7 = Just DNSQTypeRelMB
--dnsCodeToQType {ty = DNSQTypeMG} 8 = Just DNSQTypeRelMG
--dnsCodeToQType {ty = DNSQTypeMR} 9 = Just DNSQTypeRelMR
--dnsCodeToQType {ty = DNSQTypeWKS} 11 = Just DNSQTypeRelWKS
--dnsCodeToQType {ty = DNSQTypeHINFO} 13 = Just DNSQTypeRelHINFO
--dnsCodeToQType {ty = DNSQTypeMINFO} 14 = Just DNSQTypeRelMINFO

dnsQTypeToCode : {code : Int} -> (ty : DNSQType) -> DNSQTypeRel code ty
dnsQTypeToCode {code = 1} DNSQTypeA = DNSQTypeRelA
dnsQTypeToCode {code = 2} DNSQTypeNS = DNSQTypeRelNS
dnsQTypeToCode {code = 5} DNSQTypeCNAME = DNSQTypeRelCNAME
dnsQTypeToCode {code = 10} DNSQTypeNULL = DNSQTypeRelNULL
dnsQTypeToCode {code = 12} DNSQTypePTR = DNSQTypeRelPTR
dnsQTypeToCode {code = 15} DNSQTypeMX = DNSQTypeRelMX
dnsQTypeToCode {code = 16} DNSQTypeTXT = DNSQTypeRelTXT
dnsQTypeToCode {code = 28} DNSQTypeAAAA = DNSQTypeRelAAAA
dnsQTypeToCode {code = 252} DNSQTypeAXFR = DNSQTypeRelAXFR
dnsQTypeToCode {code = 253} DNSQTypeMAILB = DNSQTypeRelMAILB 
dnsQTypeToCode {code = 254} DNSQTypeMAILA = DNSQTypeRelMAILA 
dnsQTypeToCode {code = 255} DNSQTypeALL = DNSQTypeRelALL
--dnsQTypeToCode {code = 3} DNSQTypeMD = DNSQTypeRelMD
--dnsQTypeToCode {code = 4} DNSQTypeMF = DNSQTypeRelMF
--dnsQTypeToCode {code = 6} DNSQTypeSOA = DNSQTypeRelSOA
--dnsQTypeToCode {code = 7} DNSQTypeMB = DNSQTypeRelMB
--dnsQTypeToCode {code = 8} DNSQTypeMG = DNSQTypeRelMG
--dnsQTypeToCode {code = 9} DNSQTypeMR = DNSQTypeRelMR
--dnsQTypeToCode {code = 11} DNSQTypeWKS = DNSQTypeRelWKS
--dnsQTypeToCode {code = 13} DNSQTypeHINFO = DNSQTypeRelHINFO
--dnsQTypeToCode {code = 14} DNSQTypeMINFO = DNSQTypeRelMINFO

data DNSResponse = DNSResponseNoError
                 | DNSResponseFormatError
                 | DNSResponseServerError
                 | DNSResponseNameError
                 | DNSResponseNotImplementedError
                 | DNSResponseRefusedError

data DNSResponseRel : Int -> DNSResponse -> Type where
  DNSResponseRelNoErr : DNSResponseRel 0 DNSResponseNoError
  DNSResponseRelFormatErr : DNSResponseRel 1 DNSResponseFormatError 
  DNSResponseRelServerErr : DNSResponseRel 2 DNSResponseServerError
  DNSResponseRelNameErr : DNSResponseRel 3 DNSResponseNameError
  DNSResponseRelNotImplErr : DNSResponseRel 4 DNSResponseNotImplementedError
  DNSResponseRelRefusedErr : DNSResponseRel 5 DNSResponseRefusedError

dnsResponseToCode : {code : Int} -> (ty : DNSResponse) -> DNSResponseRel code ty
dnsResponseToCode {code = 0} DNSResponseNoError = DNSResponseRelNoErr
dnsResponseToCode {code = 1} DNSResponseFormatError = DNSResponseRelFormatErr
dnsResponseToCode {code = 2} DNSResponseServerError = DNSResponseRelServerErr
dnsResponseToCode {code = 3} DNSResponseNameError = DNSResponseRelNameErr
dnsResponseToCode {code = 4} DNSResponseNotImplementedError = DNSResponseRelNotImplErr
dnsResponseToCode {code = 5} DNSResponseRefusedError = DNSResponseRelRefusedErr

dnsCodeToResponse : (code : Int) -> DNSResponseRel code ty
dnsCodeToResponse {ty = DNSResponseNoError} 0 = DNSResponseRelNoErr
dnsCodeToResponse {ty = DNSResponseFormatError} 1 = DNSResponseRelFormatErr
dnsCodeToResponse {ty = DNSResponseServerError} 2 = DNSResponseRelServerErr
dnsCodeToResponse {ty = DNSResponseNameError} 3 = DNSResponseRelNameErr
dnsCodeToResponse {ty = DNSResponseNotImplementedError} 4 = DNSResponseRelNotImplErr
dnsCodeToResponse {ty = DNSResponseRefusedError} 5 = DNSResponseRelRefusedErr



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

getPayloadRel : {pl_ty : DNSPayloadType} ->
              (ty : DNSType) -> 
              (cls : DNSClass) -> 
              Maybe (DNSPayloadRel ty cls pl_ty)
getPayloadRel {pl_ty = DNSIPv4} DNSTypeA DNSClassIN = Just DNSPayloadRelIP
getPayloadRel {pl_ty = DNSIPv6} DNSTypeAAAA DNSClassIN = Just DNSPayloadRelIP6
getPayloadRel {pl_ty = DNSDomain} DNSTypeCNAME DNSClassIN = Just DNSPayloadRelCNAME
getPayloadRel {pl_ty = DNSDomain} DNSTypeNS DNSClassIN = Just DNSPayloadRelNS
getPayloadRel _ _ = Nothing

getPayloadRel' : (ty_rel : DNSTypeRel ty_code ty) ->
                 (cls_rel : DNSClassRel cls_code cls) ->
                 Maybe (DNSPayloadRel ty cls pl_ty)
getPayloadRel' {pl_ty = DNSIPv4} DNSTypeRelA DNSClassRelIN = Just DNSPayloadRelIP
getPayloadRel' {pl_ty = DNSDomain} DNSTypeRelNS DNSClassRelIN = Just DNSPayloadRelNS 
getPayloadRel' {pl_ty = DNSDomain} DNSTypeRelCNAME DNSClassRelIN = Just DNSPayloadRelCNAME
getPayloadRel' {pl_ty = DNSIPv6} DNSTypeRelAAAA DNSClassRelIN = Just DNSPayloadRelIP6
getPayloadRel' _ _ = Nothing
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


