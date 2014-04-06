module IdrisNet.UDP.UDPCommon
import IdrisNet.Socket

data UDPRes : (res : Type) -> Type where
  UDPSuccess : res -> UDPRes res
  UDPRecoverableError : SocketError -> UDPRes res
  UDPFailure : SocketError -> UDPRes res

