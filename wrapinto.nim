# experimental

import macros
import msgpack

macro wrapInto(t: expr): expr =
  return quote do:
    (proc (x:Msg): int =
      unwrapInt(x))

let m = Int8(11)
echo wrapInto(int)(m)
