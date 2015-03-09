# Experimental
#
# MessagePack-RPC
#

import msgpack
import asyncdispatch
import asyncnet
import rawsockets
import streams

# proc `$>`[A, B](fut: Future[A], f: proc(x: A): B): Future[B] =
#   # experimental. use await?
#   let retfut = newFuture[B]("asyncdispatch.`lift`")
#   fut.callback =
#     proc (fut: Future[A]) =
#       if fut.failed:
#         retfut.fail(fut.error)
#       else:
#         retfut.complete(f(fut.read))
#   return retfut

import tables


type Server = object
  sock: AsyncSocket
  funcs: Table[string, proc (x: seq[Msg]): Msg]
  notifies: Table[string, proc (x: seq[Msg])]

proc addFunc*(server: Server, key: string, f: proc (x: seq[Msg]): Msg) =
  discard

proc runFunc(server: Server, key: string, params: seq[Msg]): Msg =
  discard

proc addNotify*(server: Server, key: string, f: proc (x: seq[Msg])) =
  discard

proc runNotify(server: Server, key: string, params: seq[Msg]) =
  discard

proc mkServer(sock: AsyncSocket): Server =
  Server(sock: sock)

proc decompose(data: string): seq[Msg] =
  let st = newStringStream(data)
  let msg = st.unpack
  msg.unwrapArray

proc handleRequest(server: Server, conn: AsyncSocket) {.async.} =
  let data = await conn.recv(1 shl 63)
  let arr = data.decompose
  case unwrapInt(arr[0]):
  of 0:
    let id = unwrapInt(arr[1])
    let key = unwrapStr(arr[2])
    let ret = server.runFunc(key, arr[3..arr.high])
    # How do we know that this RPC failed?
    # For now, we always return Nil (meaning success)
    let arr = FixArray(@[PFixNum(1), arr[1], Nil, ret])
    var st = newStringStream()
    st.pack(arr)
    await conn.send(st.data)
  of 2:
    let key = unwrapStr(arr[1])
    server.runNotify(key, arr[2..arr.high])
  else:
    assert(false)

proc loop(server: Server) {.async.} =
  while true:
    let conn: AsyncSocket = await server.sock.accept
    asyncCheck server.handleRequest(conn)

proc run(server: Server): auto =
  asyncCheck server.loop
  runForever

type Client = object
  sock: AsyncSocket

proc Client(sock: AsyncSocket): Client =
  Client(sock: sock)

proc call(cli: Client, fun: string, params: openArray[Msg]): Future[Msg] {.async.} =
  let req = "aaa" # TODO
  await cli.sock.send(req)
  let data = await cli.sock.recv(1 shl 63)
  let arr = doHandle(data)
  result =
    case toInt(arr[0]):
    of 1:
      let id = toInt(arr[1])
      let success = arr[2].kind == mkNil
      if success:
        arr[3]
      else:
        Nil
    else:
      Nil

proc notify(cli: Client, fun: Msg, params: openArray[Msg]): Future[void] {.async.} =
  discard

import os
when isMainModule:
  let cl = commandLineParams()
  assert(len(cl) == 1)
  let t = cl[0] # server or client
  if t == "server":
    let server = TCPServer("localhost", Port(20000))
    server.addMethod(proc (x: openArray[Msg]): Msg =
      let a = unwrapInt(x)
      toMsg(a * 2))
    server.run
  elif t == "client":
    let client = TCPClient("localhost", Port(20000))
    cli
  else:
    assert(false)
