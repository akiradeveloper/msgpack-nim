# Experimental
#
# MessagePack-RPC
#

import msgpack
import asyncdispatch
import asyncnet
import rawsockets
import streams

proc lift[A, B](f: proc(x: A): B, fut: Future[A]): Future[B] =
  let retfut = newFuture[B]("asyncdispatch.`lift`")
  fut.callback =
    proc (fut: Future[A]) =
      if fut.failed:
        retfut.fail(fut.error)
      else:
        retfut.complete(fut.read)
  return retfut

type Address = object
  address: string
  port: Port

type Server = object
  sock: AsyncSocket

proc mkServer(sock: AsyncSocket): Server =
  Server(sock: sock)

proc addProc(server: Server, key: string, f: proc (x: seq[Msg]): Msg) =
  discard

proc addNotify(server: Server, key: string, f: proc (x: seq[Msg]): void) =
  discard

proc handle(server: Server, data: string) =
  let st = newStringStream(data)
  let msg = st.unpack
  let arr: seq[Msg] = case msg.kind:
    of mkFixArray:
      msg.vFixArray
    of mkArray16:
      msg.vArray16
    of mkArray32:
      msg.vArray32
    else:
      @[]
  
proc start(server: Server) {.async.} =
  while true:
    let conn: AsyncSocket = await server.sock.accept
    let data = await conn.recv(1 shl 60)
    server.handle(data)

type Client = object
  sock: AsyncSocket

proc mkClient(sock: AsyncSocket): Client =
  Client(sock: sock)

proc call(cli: Client, fun: Msg, params: openArray[Msg]): Future[Msg] {.async.} =
  discard

proc notify(cli: Client, fun: Msg, params: openArray[Msg]): Future[void] {.async.} =
  discard

import os
when isMainModule:
  let cl = commandLineParams()
  assert(len(cl) == 1)
  let t = cl[0] # server or client
  if t == "server":
    discard
  elif t == "client":
    discard
  else:
    assert(false)
