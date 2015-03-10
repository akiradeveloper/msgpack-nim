# Experimental
#
# MessagePack-RPC
#

import msgpack
import asyncdispatch
import asyncnet
import rawsockets
import streams
import tables

let READALL = 1 shl 63

type RPCMethod* = proc (args: seq[Msg]): Msg

type Server* = object
  sock: AsyncSocket
  methods: TableRef[string, proc (x: seq[Msg]): Msg]

proc mkServer*(sock: AsyncSocket): Server =
  Server(
    sock: sock,
    methods: newTable[string, RPCMethod]()
  )

proc addFunc*(server: var Server, key: string, f: RPCMethod) =
  server.methods.add(key, f)

proc decompose(data: string): seq[Msg] =
  let st = newStringStream(data)
  let msg = st.unpack
  msg.unwrapArray

proc handleRequest(server: Server, conn: AsyncSocket) {.async.} =
  let inData = await conn.recv(READALL)
  let inMsg = decompose(inData)
  case unwrapInt(inMsg[0]):
  of 0:
    let id = unwrapInt(inMsg[1])
    let key = unwrapStr(inMsg[2])
    var failed = false
    # TODO should also check arity
    if not server.methods.hasKey(key):
      failed = true
    var outMsg = Nil
    if not failed:
      let f = server.methods[key]
      let ret = f(inMsg[3..inMsg.high])
      outMsg = FixArray(@[PFixNum(1), inMsg[1], Nil, ret])
    else:
      outMsg = FixArray(@[PFixNum(1), inMsg[1], (-1).toMsg, Nil])
    var st = newStringStream()
    st.pack(outMsg)
    await conn.send(st.data)
  of 2:
    let key = unwrapStr(inMsg[1])
    var failed = false
    if not server.methods.hasKey(key):
      failed = true
    if failed:
      return
    let f = server.methods[key]
    discard f(inMsg[2..inMsg.high])
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

proc mkClient(sock: AsyncSocket): Client =
  Client(sock: sock)

proc call(cli: Client, fun: string, params: openArray[Msg]): Future[Msg] {.async.} =
  let reqMsg: Msg = FixArray(@[])
  var st = newStringStream()
  st.pack(reqMsg)
  await cli.sock.send(st.data)
  let ackData = await cli.sock.recv(READALL)
  let ackMsg = decompose(ackData)
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
