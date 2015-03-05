# Experimental
#
# MessagePack-RPC
#

import msgpack
import asyncdispatch
import asyncnet
import rawsockets
import streams

proc `$>`[A, B](fut: Future[A], f: proc(x: A): B): Future[B] =
  # experimental. use await?
  let retfut = newFuture[B]("asyncdispatch.`lift`")
  fut.callback =
    proc (fut: Future[A]) =
      if fut.failed:
        retfut.fail(fut.error)
      else:
        retfut.complete(f(fut.read))
  return retfut

type Server = object
  sock: AsyncSocket

proc mkServer(sock: AsyncSocket): Server =
  Server(sock: sock)

proc addProc(server: Server, key: Msg, f: proc (x: seq[Msg]): Msg) =
  discard

proc runProc(server: Server, key: Msg, params: seq[Msg]): Msg =
  discard

proc addNotify(server: Server, key: Msg, f: proc (x: seq[Msg]): void) =
  discard

proc runNotify(server: Server, key: Msg, params: seq[Msg]) =
  discard

proc toInt(m: Msg): int = discard

proc doHandle(data: string): seq[Msg] =
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

proc handle(server: Server, conn: AsyncSocket) {.async.} =
  let data = await conn.recv(1 shl 60)
  let arr = doHandle(data)
  case toInt(arr[0]):
  of 0:
    let id = toInt(arr[1])
    let key = arr[2]
    let r = server.runProc(key, arr[3..arr.high])
    let res = "aaa"
    await conn.send(res)
  of 2:
    let key = arr[1]
    server.runNotify(key, arr[2..arr.high])
  else:
    assert(false)

proc start(server: Server) {.async.} =
  while true:
    let conn: AsyncSocket = await server.sock.accept
    asyncCheck server.handle(conn)

type ClientGen = object

type Client = object
  sock: AsyncSocket

proc mkClient(gen: ClientGen, sock: AsyncSocket): Client =
  Client(sock: sock)

proc call(cli: Client, fun: Msg, params: openArray[Msg]): Future[Msg] {.async.} =
  let req = "aaa" # TODO
  await cli.sock.send(req)
  let data = await cli.sock.recv(10000)
  let arr = doHandle(data)
  result = case toInt(arr[0]):
    of 1:
      let id = toInt(arr[1])
      let success = arr[2].kind == mkNil
      if success:
        arr[3]
      else:
        Nil
    else:
      Nil

proc call2(cli, fun, params): Future[Msg] =
  result = call(cli, fun, params)
  result.callback = proc (x: Future[Msg]) =
    cli.sock.close

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
