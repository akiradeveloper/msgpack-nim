#
#                          Msgpack binding for Nim
#                     (c) Copyright 2015 Akira hayakawa
#

# ------------------------------------------------------------------------------

import endians
import unsigned

type b8 = int8

type
  MsgKind = enum
    mkNil
    mkFalse,
    mkTrue,
    mkFixArray
    mkPFixNum
    mkNFixNum
    mkU16
    mkU32
    mkU64
    mkFIxStr
  Msg* = object
    case kind: MsgKind
    of mkNil: nil
    of mkFalse: nil
    of mkTrue: nil
    of mkFixArray: vFixArray: seq[Msg]
    of mkPFixNum: vPFixNum: uint8
    of mkNFixNum: vNFixNum: uint8
    of mkU16: vU16: uint16
    of mkU32: vU32: uint32
    of mkU64: vU64: uint64
    of mkFixStr: vFixStr: string

proc Nil*(): Msg =
  Msg(kind: mkNil)

proc False*(): Msg =
  Msg(kind: mkFalse)

proc True*(): Msg =
  Msg(kind: mkTrue)

proc FixArray*(v: seq[Msg]): Msg =
  Msg(kind: mkFixArray, vFixArray: v)

proc PFixNum*(v: uint8): Msg =
  Msg(kind: mkPFixNum, vPFixNum: v)

proc NFixNum*(v: uint8): Msg =
  Msg(kind: mkNFixNum, vNFixNum: v)

proc U16*(v: uint16): Msg =
  Msg(kind: mkU16, vU16: v)

proc U32*(v: uint32): Msg =
  Msg(kind: mkU32, vU32: v)

# should take int64?
proc U64*(v: uint64): Msg =
  Msg(kind: mkU64, vU64: v)

proc FixStr*(v: string): Msg =
  Msg(kind: mkFixStr, vFixStr: v)

# should be redesigned so using seq[uint8] is presumed
# pointer arithmetics?
type PackBuf = ref object
  p: seq[uint8]
  pos: int

proc ensureMore(buf: PackBuf, addLen: int) =
  # If more buffer is required we will double the size
  if (buf.pos + addLen) >= len(buf.p):
    buf.p.setLen(len(buf.p) * 2)

proc appendBe8(buf: PackBuf, v: uint8) =
  buf.p[buf.pos] = v
  buf.pos += 1

proc fromBe8(p: pointer): uint8 =
  cast[ptr uint8](p)[]

proc fromBe16(p: pointer): int16 =
  when cpuEndian == littleEndian:
    var v: int16
    swapEndian16(addr(v), p)
    v
  else:
    copyMem(addr(v), p, 2)
    v

proc fromBe32(p: pointer): int32 =
  when cpuEndian == littleEndian:
    var v: int32
    swapEndian32(addr(v), p)
    v
  else:
    copyMem(addr(v), p, 4)
    v

proc fromBe64(p: pointer): int64 =
  when cpuEndian == littleEndian:
    var v: int64
    swapEndian64(addr(v), p)
    v
  else:
    copyMem(addr(v), p, 8)
    v

type Packer = ref object
  buf: PackBuf

proc mkPacker(buf: PackBuf): Packer =
  Packer (
    buf: buf
  )

proc pack(pc: Packer, msg: Msg) =
  let buf = pc.buf
  case msg.kind:
  of mkNil:
    echo "nil"
    buf.ensureMore(1)
    buf.appendBe8(0xc0)
  of mkFalse:
    echo "false"
    buf.ensureMore(1)
    buf.appendBe8(0xc2)
  of mkTrue:
    echo "true"
    buf.ensureMore(1)
    buf.appendBe8(0xc3)
  of mkFixArray:
    echo "fixarray"
    let h = 0x90 or len(msg.vFixArray)
    buf.ensureMore(1)
    buf.appendBe8(h.uint8)
    for e in msg.vFixArray:
      pc.pack(e)
  of mkPFixNum:
    echo "pfixnum"
    let h = 0x7f and msg.vPFixNum.int
    buf.ensureMore(1)
    buf.appendBe8(h.uint8)
  of mkNFixNum:
    echo "nfixnum"
    let h = 0xe0 or msg.vNFixNum.int
    buf.ensureMore(1)
    buf.appendBe8(h.uint8)
  of mkU16:
    echo "u16"
    buf.ensureMore(1+2)
    buf.appendBe8(0xcd)
    var v = msg.vU16
    bigEndian16(addr(buf.p[buf.pos]), addr(v))
    buf.pos += 2
  of mkU32:
    echo "u32"
    buf.ensureMore(1+4)
    buf.appendBe8(0xce)
    var v = msg.vU32
    bigEndian32(addr(buf.p[buf.pos]), addr(v))
    buf.pos += 4
  of mkU64:
    echo "u64"
    buf.ensureMore(1+8)
    buf.appendBe8(0xcf)
    var v = msg.vU64
    bigEndian64(addr(buf.p[buf.pos]), addr(v))
    buf.pos += 8
  of mkFixStr:
    echo "fixstr"
    let sz = len(msg.vFixStr)
    let h = 0xa0 or sz
    buf.ensureMore(1+sz)
    buf.appendBe8(h.uint8)
    var m = msg
    copyMem(addr(buf.p[buf.pos]), addr(m.vFixStr[0]), sz)

type UnpackBuf = ref object
  p: pointer

proc inc(buf: UnpackBuf, n:int) =
  var a = cast[ByteAddress](buf.p)
  a += n
  buf.p = cast[pointer](a)

type Unpacker = ref object
  buf: UnpackBuf

proc mkUnpacker(buf: UnpackBuf): Unpacker =
  Unpacker (
    buf: buf
  )

proc unpack(upc: Unpacker): Msg =
  let buf = upc.buf
  let h = fromBe8(buf.p)
  inc(buf, 1)

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
  of 0x00..0x7f:
    echo "pfixnum"
    let v = h.int and 0x7f
    PFixNum(v.uint8)
  of 0xe0..0xff:
    echo "nfixnum"
    let v = h.int and 0x1f
    NFixNum(v.uint8)
  of 0xcd:
    echo "u16"
    let v = fromBe16(buf.p)
    buf.inc(2)
    U16(cast[uint16](v))
  of 0xce:
    echo "u32"
    let v = fromBe32(buf.p)
    buf.inc(4)
    U32(cast[uint32](v))
  of 0xcf:
    echo "u64"
    let v = fromBe64(buf.p)
    buf.inc(8)
    U64(cast[uint64](v))
  of 0xa0..0xbf:
    echo "fixstr"
    let sz = h.int and 0x1f
    var s = newString(sz)
    copyMem(addr(s[0]), buf.p, sz)
    buf.inc(sz)
    FixStr(s)
  else:
    Nil() # tmp

# At the initial release we won't open interfaces
# other than the followings.

proc pack*(msg: Msg): tuple[p: pointer, size: int] =
  let packBuf = PackBuf(p: newSeq[uint8](128), pos: 0)
  let pc = mkPacker(packBuf)
  pc.pack(msg)
  tuple(p: cast[pointer](addr(packBuf.p[0])), size: packBuf.pos)

proc unpack*(p: pointer): Msg =
  let unpackbuf = UnpackBuf(p:p)
  let upc = mkUnpacker(unpackBuf)
  upc.unpack()

# ------------------------------------------------------------------------------

proc t*(msg: Msg) =
  ## Test by cyclic translation
  let (p, sz) = pack(msg)
  discard sz
  let unpacked = unpack(p)
  assert($expr(msg) == $expr(unpacked))

when isMainModule:
  t(Nil())
  t(False())
  t(True())
  t(FixArray(@[True(), False()]))
  t(PFixNum(127))
  t(NFixNum(31))
  t(U16(10000))
  t(U32(10000))
  t(U64(10000))
  t(FixStr("akiradeveloper"))
