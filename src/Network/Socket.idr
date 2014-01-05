-- Time to do this properly.
-- Low-Level C Sockets bindings for Idris. Used by higher-level, cleverer things.
-- (C) SimonJF, MIT Licensed, 2014
module Network.Socket

%include C "idrisnet.h"
%include C "<sys/types.h>" -- Pushing my luck, might need to re-export everything
%include C "<sys/sockets.h>" 
%include C "<netdb.h>"
-- %link C "idrisnet.o"

ByteLength : Type
ByteLength = Int

class ToCode a where
  toCode : a -> Int

-- Socket Families.
-- The ones that people might actually use. We're not going to need US Government
-- proprietary ones...
data SocketFamily = AF_UNSPEC -- Unspecified
                  | AF_INET   -- IP / UDP etc. IPv4
                  | AF_INET6  -- IP / UDP etc. IPv6

instance Show SocketFamily where
  show AF_UNSPEC = "AF_UNSPEC"
  show AF_INET   = "AF_INET"
  show AF_INET6  = "AF_INET4"

instance ToCode SocketFamily where
  toCode AF_UNSPEC = 0
  toCode AF_INET   = 2
  toCode AF_INET6  = 10

-- Socket Types.
data SocketType = NotASocket  -- Not a socket, used in certain operations
                | Stream      -- TCP
                | Datagram    -- UDP
                | Raw         -- Raw sockets. A guy can dream.

instance Show SocketType where
  show NotASocket = "Not a socket"
  show Stream     = "Stream"
  show Datagram   = "Datagram"
  show Raw        = "Raw"

instance ToCode SocketType where
  toCode NotASocket = 0
  toCode Stream     = 1
  toCode Datagram   = 2
  toCode Raw        = 3

-- Protocol Number. Generally good enough to just set it to 0.
ProtocolNumber : Type
ProtocolNumber = Int

-- SocketError: Error thrown by a socket operation
SocketError : Type
SocketError = Int

-- SocketDescriptor: Native C Socket Descriptor
SocketDescriptor : Type
SocketDescriptor = Int

data SocketAddress = IPv4Addr Int Int Int Int
                   | IPv6Addr -- Not implemented (yet)
                   | Hostname String

instance Show SocketAddress where
  show (IPv4Addr i1 i2 i3 i4) = concat $ Prelude.List.intersperse "." (map show [i1, i2, i3, i4])
  show IPv6Addr = "NOT IMPLEMENTED YET"
  show (Hostname host) = host

Port : Type
Port = Int

-- Allocates an amount of memory given by the ByteLength parameter.
-- Used to allocate a mutable pointer to be given to the Recv functions.
private
alloc : ByteLength -> IO Ptr
alloc bl = mkForeign (FFun "idrnet_malloc" [FInt] FPtr) bl

record Socket : Type where
  MkSocket : (descriptor : SocketDescriptor) ->
             (family : SocketFamily) ->
             (socketType : SocketType) ->
             (protocolNumber : ProtocolNumber) ->
             Socket

getErrno : IO Int
getErrno = mkForeign (FFun "idrnet_errno" [] FInt)

-- Creates a UNIX socket with the given family, socket type and protocol number.
-- Returns either a socket or an error.
socket : SocketFamily -> SocketType -> ProtocolNumber -> IO (Either SocketError Socket)
socket sf st pn = do
  socket_res <- mkForeign (FFun "socket" [FInt, FInt, FInt] FInt) (toCode sf) (toCode st) pn
  if socket_res == -1 then -- error
    map Left getErrno
  else 
    return $ Right (MkSocket socket_res sf st pn)


-- Binds a socket to the given socket address and port.
-- Returns 0 on success, an error code otherwise.
bind : Socket -> SocketAddress -> Port -> IO Int
bind sock addr port = do
  bind_res <- (mkForeign (FFun "idrnet_bind" [FInt, FString, FInt] FInt) 
                           (descriptor sock) (show addr) port)
  if bind_res == (-1) then -- error
    getErrno
  else
    return 0 -- Success


