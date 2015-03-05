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

proc addProc(server: Server) =
  discard

proc addNotify(server: Server) =
  discard

proc handle(data: string) =
  let st = newStringStream(data)
  let msg = st.unpack
  let arr: seq[Msg] = case msg.kind:
    of mkFixArray:

    of mkArray16:
    of mkArray32:


proc start(server: Server) {.async.} =
  while true:
    let conn: AsyncSocket = await sock.accept
    let data = await conn.recv(1 shl 60)
    server.handle(data)

type Client = object
  sock: AsyncSocket

proc mkClient(sock: AsyncSocket): Client =
  Client(sock: sock)

proc call(cli: Client, fun: Msg, params: openArray[Msg]): Future[Msg] =
  discard

proc notify(cli: Client, fun: Msg, params: openArray[Msg]): Future[void] =
  discard

