# Experiment

import asyncdispatch
import asyncnet
import rawsockets

let sock = newAsyncSocket(buffered=false)
asyncCheck sock.connect(address="127.0.0.1", port=Port(20001))

proc run(sock: AsyncSocket) {.async.} =
  await sock.send("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

# waitFor(run(sock)) # OK (this is looping with polling)

let fu = run(sock)
asyncCheck fu
while not fu.finished:
  discard # NG
  # poll() # OK
