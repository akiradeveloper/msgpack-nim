#
#                          Msgpack binding for Nim
#                     (c) Copyright 2015 Akira hayakawa
#

# ------------------------------------------------------------------------------

import unsigned

type
  MsgKind = enum
    mkNil
    mkFalse,
    mkTrue,
    mkFixArray
  Msg = object
    case kind: MsgKind
    of mkNil: nil
    of mkFalse: nil
    of mkTrue: nil
    of mkFixArray: v: seq[Msg]

proc Nil(): Msg =
  Msg(kind: mkNil)

proc False(): Msg =
  Msg(kind: mkFalse)

proc True(): Msg =
  Msg(kind: mkTrue)

proc FixArray(v: seq[Msg]): Msg =
  Msg(kind: mkFixArray, v: v)

# should be redesigned so using seq[uint8] is presumed
# pointer arithmetics?
type Buffer = ref object
  raw: seq[uint8]
  pos: int

proc appendBe8(buf: Buffer, v: uint8) =
  buf.raw[buf.pos] = v
  buf.pos += 1

type Packer = ref object
  buf: Buffer

proc mkPacker(buf: Buffer): Packer =
  Packer (
    buf: buf
  )

proc pack(pc: Packer, msg: Msg) =
  case msg.kind:
  of mkNil:
    echo "nil"
    pc.buf.appendBe8(0xc0)
  of mkFalse:
    echo "false"
    pc.buf.appendBe8(0xc2)
  of mkTrue:
    echo "true"
    pc.buf.appendBe8(0xc3)
  of mkFixArray:
    echo "fixarray"
    let h = 0x90 or len(msg.v)
    pc.buf.appendBe8(h.uint8)
    for e in msg.v:
      pc.pack(e)

type Unpacker = ref object
  buf: Buffer

proc mkUnpacker(buf: Buffer): Unpacker =
  Unpacker (
    buf: buf
  )

proc unpack(upc: Unpacker): Msg =
  let h = upc.buf.raw[upc.buf.pos]
  upc.buf.pos += 1
  echo h
  case h
  of 0xc0:
    echo "nil"
    Nil()
  of 0xc2:
    echo "false"
    False()
  of 0xc3:
    echo "true"
    True()
  of 0x90..0x9f: # uint8
    echo "fixarray"
    let sz: uint8 = h and 0x0f
    var v: seq[Msg] = @[]
    for i in 0..(sz-1):
      v.add(upc.unpack())
    FixArray(v)
  else:
    Nil() # tmp

# ------------------------------------------------------------------------------

template t(msg: Msg) =
  let buf = Buffer (
    raw: newSeq[uint8](128),
    pos: 0
  )
  let pc = mkPacker(buf)
  pc.pack(msg)

  buf.pos = 0
  let upc = mkUnpacker(buf)
  let unpacked = upc.unpack()
  echo expr(unpacked)
  assert($expr(msg) == $expr(unpacked))

when isMainModule:
  t(Nil())
  t(False())
  t(True())
  t(FixArray(@[True(), False()]))
