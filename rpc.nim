# Experimental
#
# MessagePack-RPC
#

import msgpack
import asyncdispatch
import asyncnet
import rawsockets
import streams

type Server = object
  sock: AsyncSocket

proc mkServer(sock: AsyncSocket): Server =
  Server(sock: sock)

proc start(server: Server) {.async.} =
  discard

type Client = object
  sock: AsyncSocket

proc mkClient(sock: AsyncSocket): Client =
  Client(sock: sock)

proc call(cli: Client, fun: Msg, params: openArray[Msg]): Future[Msg] =
  discard

proc notify(cli: Client, fun: Msg, params: openArray[Msg]): Future[void] =
  discard

