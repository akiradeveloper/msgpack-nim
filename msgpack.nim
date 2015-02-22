#
#                          Msgpack binding for Nim
#                     (c) Copyright 2015 Akira hayakawa
#

# ------------------------------------------------------------------------------

type
  MsgKind = enum
    mkNil
  Msg = object
    case kind: MsgKind
    of mkNil: nil

type Bin = seq[uint8]

proc Nil(): Msg =
  Msg(kind: mkNil)

proc pack(msg: Msg): Bin =
  @[]
  
proc unpack(s: Bin): Msg =
  Nil()

# ------------------------------------------------------------------------------

template t(msg: Msg) =
  let packed = pack(msg)
  let unpacked = unpack(packed)
  assert($expr(Nil()) == $expr(unpacked))

when isMainModule:
  t(Nil())
