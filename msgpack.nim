#
#                        MessagePack binding for Nim
#                     (c) Copyright 2015 Akira hayakawa
#

# ------------------------------------------------------------------------------

import endians
import unsigned

type
 b8*  = int8
 b16 = int16
 b32 = int32
 b64 = int64

type
  MsgKind = enum
    mkNil
    mkFalse
    mkTrue
    mkFixArray
    mkArray16
    mkArray32
    mkFixMap
    mkMap16
    mkMap32
    mkPFixNum
    mkNFixNum
    mkU16
    mkU32
    mkU64
    mkFixStr
    mkStr32
    mkFloat32
    mkFloat64
    mkFixExt1
    mkExt32
    mkBin8
  Msg = ref MsgObj
  MsgObj = object
    case kind: MsgKind
    of mkNil: nil
    of mkFalse: nil
    of mkTrue: nil
    of mkFixArray: vFixArray: seq[Msg]
    of mkArray16: vArray16: seq[Msg]
    of mkArray32: vArray32: seq[Msg]
    of mkFixMap: vFixMap: seq[tuple[key:Msg, val:Msg]]
    of mkMap16: vMap16: seq[tuple[key:Msg, val:Msg]]
    of mkMap32: vMap32: seq[tuple[key:Msg, val:Msg]]
    of mkPFixNum: vPFixNum: uint8
    of mkNFixNum: vNFixNum: uint8
    of mkU16: vU16: uint16
    of mkU32: vU32: uint32
    of mkU64: vU64: uint64
    of mkFixStr: vFixStr: string
    of mkStr32: vStr32: string
    of mkFloat32: vFloat32: float32
    of mkFloat64: vFloat64: float64
    of mkBin8: vBin8: seq[b8]
    of mkExt32:
      typeExt32: uint8
      vExt32: seq[b8]
    of mkFixExt1:
      typeFixExt1: uint8
      vFixExt1: seq[b8] # should be array[1, b8]

proc `$`(msg: Msg): string =
  $(msg[])

proc Map16*(v: seq[tuple[key: Msg, val: Msg]]): Msg =
  Msg(kind: mkMap16, vMap16: v)

proc Map32*(v: seq[tuple[key: Msg, val: Msg]]): Msg =
  Msg(kind: mkMap32, vMap32: v)

proc Str32*(s: string): Msg =
  Msg(kind: mkStr32, vStr32: s)

proc Nil*(): Msg =
  Msg(kind: mkNil)

proc False*(): Msg =
  Msg(kind: mkFalse)

proc True*(): Msg =
  Msg(kind: mkTrue)

proc FixArray*(v: seq[Msg]): Msg =
  assert(len(v) < 16)
  Msg(kind: mkFixArray, vFixArray: v)

proc Array16*(v: seq[Msg]): Msg =
  assert(len(v) < (1 shl 16))
  Msg(kind: mkArray16, vArray16: v)

proc Array32*(v: seq[Msg]): Msg =
  assert(len(v) < (1 shl 32))
  Msg(kind: mkArray32, vArray32: v)

proc PFixNum*(v: uint8): Msg =
  assert(v < 128)
  Msg(kind: mkPFixNum, vPFixNum: v)

proc NFixNum*(v: uint8): Msg =
  assert(v < 32)
  Msg(kind: mkNFixNum, vNFixNum: v)

proc U16*(v: uint16): Msg =
  Msg(kind: mkU16, vU16: v)

proc U32*(v: uint32): Msg =
  Msg(kind: mkU32, vU32: v)

proc U64*(v: uint64): Msg =
  Msg(kind: mkU64, vU64: v)

proc FixStr*(v: string): Msg =
  assert(len(v) < 32)
  Msg(kind: mkFixStr, vFixStr: v)

proc FixMap*(v: seq[tuple[key: Msg, val: Msg]]): Msg =
  assert(len(v) < 16)
  Msg(kind: mkFixMap, vFixMap: v)

proc Float32*(v: float32): Msg =
  Msg(kind: mkFloat32, vFloat32: v)

proc Float64*(v: float64): Msg =
  Msg(kind: mkFloat64, vFloat64: v)

proc FixExt1*(t: uint8, data: seq[b8]): Msg =
  assert(len(data) == 1)
  Msg(kind: mkFixExt1, typeFixExt1: t, vFixExt1: data)

proc Ext32*(t: uint8, data: seq[b8]): Msg =
  assert(len(data) < (1 shl 32))
  Msg(kind: mkExt32, typeExt32: t, vExt32: data)

proc Bin8*(v: seq[b8]): Msg =
  assert(len(v) < (1 shl 8))
  Msg(kind: mkBin8, vBin8: v)

type PackBuf = ref object
  p: seq[b8]
  pos: int

proc ensureMore(buf: PackBuf, addLen: int) =
  # If more buffer is required we will double the size
  if unlikely((buf.pos + addLen) >= len(buf.p)):
    buf.p.setLen((len(buf.p) + addLen) * 2)

proc appendBe8(buf: PackBuf, v: b8) =
  buf.p[buf.pos] = v
  buf.pos += 1

proc appendHeader(buf: PackBuf, v: int) =
  buf.appendBe8(cast[b8](v))

proc appendBe16(buf: PackBuf, v: b16) =
  var vv = v
  bigEndian16(addr(buf.p[buf.pos]), addr(vv))
  buf.pos += 2

proc appendBe32(buf: PackBuf, v: b32) =
  var vv = v
  bigEndian32(addr(buf.p[buf.pos]), addr(vv))
  buf.pos += 4

proc appendBe64(buf: PackBuf, v: b64) =
  var vv = v
  bigEndian64(addr(buf.p[buf.pos]), addr(vv))
  buf.pos += 8

proc appendData(buf: PackBuf, p: pointer, size: int) =
  copyMem(addr(buf.p[buf.pos]), p, size)
  buf.pos += size

type Packer = ref object
  buf: PackBuf

proc mkPacker(buf: PackBuf): Packer =
  Packer (
    buf: buf
  )

proc pack(pc: Packer, msg: Msg)

proc appendArray(pc: Packer, xs: seq[Msg]) =
  for e in xs:
    pc.pack(e)

proc appendMap(pc: Packer, map: seq[tuple[key:Msg, val:Msg]]) =
  for e in map:
    pc.pack(e.key)
    pc.pack(e.val)

proc pack(pc: Packer, msg: Msg) =
  let buf = pc.buf
  case msg.kind:
  of mkStr32:
    echo "str32"
    let sz = len(msg.vStr32)
    buf.ensureMore(5 + sz)
    buf.appendHeader(0xdb)
    buf.appendBe32(cast[b32](sz.toU32))
    var m = msg
    buf.appendData(addr(m.vStr32[0]), sz)
  of mkNil:
    echo "nil"
    buf.ensureMore(1)
    buf.appendHeader(0xc0)
  of mkFalse:
    echo "false"
    buf.ensureMore(1)
    buf.appendHeader(0xc2)
  of mkTrue:
    echo "true"
    buf.ensureMore(1)
    buf.appendHeader(0xc3)
  of mkFixArray:
    echo "fixarray"
    let h: int = 0x90 or len(msg.vFixArray)
    buf.ensureMore(1)
    buf.appendHeader(h)
    pc.appendArray(msg.vFixArray)
  of mkArray16:
    echo "array16"
    let sz = len(msg.vArray16)
    buf.ensureMore(3)
    buf.appendHeader(0xdc)
    buf.appendBe16(cast[b16](sz.toU16))
    pc.appendArray(msg.vArray16)
  of mkArray32:
    echo "array32"
    let sz = len(msg.vArray32)
    buf.ensureMore(5)
    buf.appendHeader(0xdd)
    buf.appendBe32(cast[b32](sz.toU32))
    pc.appendArray(msg.vArray32)
  of mkPFixNum:
    echo "pfixnum"
    let h: int = 0x7f and msg.vPFixNum.int
    buf.ensureMore(1)
    buf.appendHeader(h)
  of mkNFixNum:
    echo "nfixnum"
    let h: int = 0xe0 or msg.vNFixNum.int
    buf.ensureMore(1)
    buf.appendHeader(h)
  of mkU16:
    echo "u16"
    buf.ensureMore(1+2)
    buf.appendHeader(0xcd)
    buf.appendBe16(cast[b16](msg.vU16))
  of mkU32:
    echo "u32"
    buf.ensureMore(1+4)
    buf.appendHeader(0xce)
    buf.appendBe32(cast[b32](msg.vU32))
  of mkU64:
    echo "u64"
    buf.ensureMore(1+8)
    buf.appendHeader(0xcf)
    buf.appendBe64(cast[b64](msg.vU64))
  of mkFixStr:
    echo "fixstr"
    let sz: int = len(msg.vFixStr)
    let h = 0xa0 or sz
    buf.ensureMore(1+sz)
    buf.appendHeader(h)
    var m = msg
    buf.appendData(addr(m.vFixStr[0]), sz)
  of mkFixMap:
    echo "fixmap"
    let sz = len(msg.vFixMap)
    let h = 0x80 or sz
    buf.ensureMore(1)
    buf.appendHeader(h)
    pc.appendMap(msg.vFixMap)
  of mkMap16:
    echo "map16"
    let sz = len(msg.vMap16)
    buf.ensureMore(3)
    buf.appendHeader(0xde)
    buf.appendBe16(cast[b16](sz.toU16))
    pc.appendMap(msg.vMap16)
  of mkMap32:
    echo "map32"
    let sz = len(msg.vMap32)
    buf.ensureMore(5)
    buf.appendHeader(0xdf)
    buf.appendBe32(cast[b32](sz.toU32))
    pc.appendMap(msg.vMap32)
  of mkFloat32:
    echo "float32"
    buf.ensureMore(1+4)
    buf.appendHeader(0xca)
    buf.appendBe32(cast[b32](msg.vFloat32))
  of mkFloat64:
    echo "float64"
    buf.ensureMore(1+8)
    buf.appendHeader(0xcb)
    buf.appendBe64(cast[b64](msg.vFloat64))
  of mkExt32:
    echo "ext32"
    let sz = len(msg.vExt32)
    buf.ensureMore(1+4+1+sz)
    buf.appendHeader(0xc9)
    buf.appendBe32(cast[b32](sz.toU32))
    buf.appendBe8(cast[b8](msg.typeExt32))
    var m = msg
    buf.appendData(addr(m.vExt32[0]), sz)
  of mkBin8:
    echo "bin8"
    let sz = len(msg.vBin8)
    buf.ensureMore(1+1+sz)
    buf.appendHeader(0xc4)
    buf.appendBe8(cast[b8](sz.toU8))
    var m = msg
    buf.appendData(addr(m.vBin8[0]), sz)
  of mkFixExt1:
    echo "fixext1"
    buf.ensureMore(3)
    buf.appendHeader(0xd4)
    buf.appendBe8(cast[b8](msg.typeFixExt1))
    var m = msg
    buf.appendData(addr(m.vFixExt1[0]), 1)

type UnpackBuf = ref object
  p: pointer

proc inc(buf: UnpackBuf, n:int) =
  var a = cast[ByteAddress](buf.p)
  a += n
  buf.p = cast[pointer](a)

proc fromBe8(p: pointer): b8 =
  cast[ptr b8](p)[]

proc fromBe16(p: pointer): b16 =
  var v: b16
  when cpuEndian == littleEndian:
    swapEndian16(addr(v), p)
    v
  else:
    copyMem(addr(v), p, 2)
    v

proc fromBe32(p: pointer): b32 =
  var v: b32
  when cpuEndian == littleEndian:
    swapEndian32(addr(v), p)
    v
  else:
    copyMem(addr(v), p, 4)
    v

proc fromBe64(p: pointer): b64 =
  var v: b64
  when cpuEndian == littleEndian:
    swapEndian64(addr(v), p)
    v
  else:
    copyMem(addr(v), p, 8)
    v

proc popBe8(buf): auto =
  result = fromBe8(buf.p)
  buf.inc(1)

proc popBe16(buf): auto =
  result = fromBe16(buf.p)
  buf.inc(2)

proc popBe32(buf): auto =
  result = fromBe32(buf.p)
  buf.inc(4)

proc popBe64(buf): auto =
  result = fromBe64(buf.p)
  buf.inc(8)

proc popData(buf: UnpackBuf, p: pointer, size: int) =
  copyMem(p, buf.p, size)
  buf.inc(size)
  

type Unpacker = ref object
  buf: UnpackBuf

proc unpack(upc: Unpacker): Msg

proc popArray(upc: Unpacker, size: int): seq[Msg] =
  var v: seq[Msg] = @[]
  for i in 0..(size-1):
    v.add(upc.unpack)
  v

proc popMap(upc: Unpacker, size: int): seq[tuple[key:Msg, val:Msg]] =
  var v: seq[tuple[key: Msg, val:Msg]] = @[]
  for i in 0..(size-1):
    v.add((upc.unpack, upc.unpack))
  v

proc mkUnpacker(buf: UnpackBuf): Unpacker =
  Unpacker (
    buf: buf
  )

proc unpack(upc: Unpacker): Msg =
  let buf = upc.buf

  let h = cast[uint8](buf.popBe8)
  echo h.int

  case h
  of 0xdb:
    echo "str32"
    let sz = cast[uint32](buf.popBe32)
    var s = newString(sz.int)
    buf.popData(addr(s[0]), sz.int)
    Str32(s)
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
    let sz: int = h and 0x0f
    FixArray(upc.popArray(sz))
  of 0xdc:
    echo "array16"
    let sz = cast[uint16](buf.popBe16)
    # var v: seq[Msg] = @[]
    # for i in 0..(sz-1):
    #   v.add(upc.unpack())
    Array16(upc.popArray(sz.int))
  of 0xdd:
    echo "array32"
    let sz = cast[uint32](buf.popBe32)
    Array32(upc.popArray(sz.int))
  of 0x80..0x8f:
    echo "fixmap"
    let sz: int = h and 0x0f
    FixMap(upc.popMap(sz))
  of 0xde:
    echo "map16"
    let sz: int = cast[int](buf.popBe16)
    Map16(upc.popMap(sz))
  of 0xdf:
    echo "map32"
    let sz: int = cast[int](buf.popBe32)
    Map32(upc.popMap(sz))
  of 0x00..0x7f:
    echo "pfixnum"
    let v: int = h and 0x7f
    PFixNum(cast[uint8](v.toU8))
  of 0xe0..0xff:
    echo "nfixnum"
    let v: int = h and 0x1f
    NFixNum(cast[uint8](v.toU8))
  of 0xcd:
    echo "u16"
    U16(cast[uint16](buf.popBe16))
  of 0xce:
    echo "u32"
    U32(cast[uint32](buf.popBe32))
  of 0xcf:
    echo "u64"
    U64(cast[uint64](buf.popBe64))
  of 0xa0..0xbf:
    echo "fixstr"
    let sz: int = h.int and 0x1f
    var s = newString(sz)
    buf.popData(addr(s[0]), sz)
    FixStr(s)
  of 0xca:
    echo "float32"
    Float32(cast[float32](buf.popBe32))
  of 0xcb:
    echo "float64"
    Float64(cast[float64](buf.popBe64))
  of 0xc9:
    echo "ext32"
    let sz = cast[uint32](buf.popBe32)
    let t = cast[uint8](buf.popBe8)
    var d = newSeq[b8](sz.int)
    buf.popData(addr(d[0]), sz.int)
    Ext32(t, d)
  of 0xc4:
    echo "bin8"
    let sz = cast[uint8](buf.popBe8)
    var d = newSeq[b8](sz.int)
    buf.popData(addr(d[0]), sz.int)
    Bin8(d)
  else:
    assert(false) # not reachable
    Nil()

# At the initial release we won't open interfaces
# other than the followings.

proc pack*(msg: Msg): tuple[p: pointer, size: int] =
  ## Serialize message to byte sequence
  let packBuf = PackBuf(p: newSeq[b8](128), pos: 0)
  let pc = mkPacker(packBuf)
  pc.pack(msg)
  tuple(p: cast[pointer](addr(packBuf.p[0])), size: packBuf.pos)

proc unpack*(p: pointer): Msg =
  ## Deserialize byte sequence to message
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
  t(Array16(@[True(), False()]))
  t(Array32(@[True(), False()]))
  t(PFixNum(127))
  t(NFixNum(31))
  t(U16(10000))
  t(U32(10000))
  t(U64(10000))
  t(FixStr("akiradeveloper"))
  t(Str32("akiradeveloper"))
  t(FixMap(@[(Nil(),True()),(False(),U16(1))]))
  t(Map16(@[(Nil(),True()),(False(),U16(1))]))
  t(Map32(@[(Nil(),True()),(False(),U16(1))]))
  t(Float32(0.12345'f32))
  t(Float64(0.78901'f64))
  t(Ext32(12, @[cast[b8](1),2,3]))
  t(Bin8(@[cast[b8](4),5,6]))
