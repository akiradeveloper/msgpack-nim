# Experiment

# import msgpack
# import asyncdispatch
# import asyncnet
# import rawsockets
# import streams
#
# type Server = object
#   sock: AsyncSocket
#
# proc mkServer(sock: AsyncSocket): Server =
#   Server(sock: sock)
#
#
# type Client = object
#   sock: AsyncSocket
#
# proc mkClient(sock: AsyncSocket): Client =
#   Client(sock: sock)
#
# proc call(cli: Client, fun: Msg, params: openArray[Msg]): Future[Msg] =
#   discard
#
# proc notify(cli: Client, fun: Msg, params: openArray[Msg]): Future[void] =
#   discard

import asyncdispatch
import asyncnet
import rawsockets

proc run(sock: AsyncSocket) {.async.} =
  while true:
    let conn: AsyncSocket = await sock.accept
    echo conn.isClosed #=> false
    let data: string = await conn.recv(10000)
    echo conn.isClosed #=> false
    echo len(data) #=> 0

proc serve() {.async.} =
  let sock = newAsyncSocket(buffered=false)
  sock.bindAddr(address="127.0.0.1", port=Port(20001))
  sock.listen()
  await sock.run()

asyncCheck serve()
runForever()
