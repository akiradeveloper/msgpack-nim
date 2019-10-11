#
#                        MessagePack binding for Nim
#                     (c) Copyright 2015 Akira hayakawa
#

import sequtils
import streams
import endians

#
# MessagePack Core
# Type, Pack/Unpack
#

type
  b16 = uint16
  b32 = uint32
  b64 = uint64

# typeではなくてkindの方が良さそう
type Ext* = tuple[kind:int, data:seq[byte]]

type
  MsgKind* = enum
    mkFixArray
    mkArray16
    mkArray32
    mkFixMap
    mkMap16
    mkMap32
    mkNil
    mkTrue
    mkFalse
    mkPFixNum
    mkNFixNum
    mkUInt8
    mkUInt16
    mkUInt32
    mkUInt64
    mkInt8
    mkInt16
    mkInt32
    mkInt64
    mkFloat32
    mkFloat64
    mkFixStr
    mkStr8
    mkStr16
    mkStr32
    mkBin8
    mkBin16
    mkBin32
    mkFixExt1
    mkFixExt2
    mkFixExt4
    mkFixExt8
    mkFixExt16
    mkExt8
    mkExt16
    mkExt32
  Msg* = ref MsgObj
  MsgObj* {.acyclic.} = object
    case kind*: MsgKind
    of mkFixArray: vFixArray*: seq[Msg]
    of mkArray16: vArray16*: seq[Msg]
    of mkArray32: vArray32*: seq[Msg]
    of mkFixMap: vFixMap*: seq[tuple[key:Msg, val:Msg]]
    of mkMap16: vMap16*: seq[tuple[key:Msg, val:Msg]]
    of mkMap32: vMap32*: seq[tuple[key:Msg, val:Msg]]
    of mkNil: nil
    of mkTrue: nil
    of mkFalse: nil
    of mkPFixNum: vPFixNum*: uint8
    of mkNFixNum: vNFixNum*: int8
    of mkUInt8: vUInt8*: uint8
    of mkUInt16: vUInt16*: uint16
    of mkUInt32: vUInt32*: uint32
    of mkUInt64: vUInt64*: uint64
    of mkInt8: vInt8*: int8
    of mkInt16: vInt16*: int16
    of mkInt32: vInt32*: int32
    of mkInt64: vInt64*: int64
    of mkFloat32: vFloat32*: float32
    of mkFloat64: vFloat64*: float64
    of mkFixStr: vFixStr*: string
    of mkStr8: vStr8*: string
    of mkStr16: vStr16*: string
    of mkStr32: vStr32*: string
    of mkBin8: vBin8*: seq[byte]
    of mkBin16: vBin16*: seq[byte]
    of mkBin32: vBin32*: seq[byte]
    of mkFixExt1: vFixExt1: Ext
    of mkFixExt2: vFixExt2: Ext
    of mkFixExt4: vFixExt4: Ext
    of mkFixExt8: vFixExt8: Ext
    of mkFixExt16: vFixExt16: Ext
    of mkExt8: vExt8: Ext
    of mkExt16: vExt16: Ext
    of mkExt32: vExt32: Ext

proc `$`*(msg: Msg): string =
  $(msg[])

# Factory methods should be inlined
{.push inline.}

proc FixArray*(v: seq[Msg]): Msg =
  assert(len(v) < 16)
  Msg(kind: mkFixArray, vFixArray: v)

proc Array16*(v: seq[Msg]): Msg =
  assert(len(v) < (1 shl 16))
  Msg(kind: mkArray16, vArray16: v)

proc Array32*(v: seq[Msg]): Msg =
  assert(len(v) < (1 shl 32))
  Msg(kind: mkArray32, vArray32: v)

proc FixMap*(v: seq[tuple[key: Msg, val: Msg]]): Msg =
  assert(len(v) < 16)
  Msg(kind: mkFixMap, vFixMap: v)

proc Map16*(v: seq[tuple[key: Msg, val: Msg]]): Msg =
  assert(len(v) < (1 shl 16))
  Msg(kind: mkMap16, vMap16: v)

proc Map32*(v: seq[tuple[key: Msg, val: Msg]]): Msg =
  assert(len(v) < (1 shl 32))
  Msg(kind: mkMap32, vMap32: v)

let
  Nil* = Msg(kind: mkNil)
  True* = Msg(kind: mkTrue)
  False* = Msg(kind: mkFalse)

# Compile type check that v is 7 bit?
proc PFixNum*(v: uint8): Msg =
  assert(v < 128)
  Msg(kind: mkPFixNum, vPFixNum: v)

proc NFixNum*(v: int8): Msg =
  assert(-32 <= v and v < 0)
  Msg(kind: mkNFixNum, vNFixNum: v)

proc UInt8*(v: uint8): Msg =
  Msg(kind: mkUInt8, vUInt8: v)

proc UInt16*(v: uint16): Msg =
  Msg(kind: mkUInt16, vUInt16: v)

proc UInt32*(v: uint32): Msg =
  Msg(kind: mkUInt32, vUInt32: v)

proc UInt64*(v: uint64): Msg =
  Msg(kind: mkUInt64, vUInt64: v)

proc Int8*(v: int8): Msg =
  Msg(kind: mkInt8, vInt8: v)

proc Int16*(v: int16): Msg =
  Msg(kind: mkInt16, vInt16: v)

proc Int32*(v: int32): Msg =
  Msg(kind: mkInt32, vInt32: v)

proc Int64*(v: int64): Msg =
  Msg(kind: mkInt64, vInt64: v)

proc Float32*(v: float32): Msg =
  Msg(kind: mkFloat32, vFloat32: v)

proc Float64*(v: float64): Msg =
  Msg(kind: mkFloat64, vFloat64: v)

proc FixStr*(v: string): Msg =
  assert(len(v) < 32)
  Msg(kind: mkFixStr, vFixStr: v)

proc Str8*(s: string): Msg =
  Msg(kind: mkStr8, vStr8: s)

proc Str16*(s: string): Msg =
  Msg(kind: mkStr16, vStr16: s)

proc Str32*(s: string): Msg =
  Msg(kind: mkStr32, vStr32: s)

proc Bin8*(v: seq[byte]): Msg =
  assert(len(v) < (1 shl 8))
  Msg(kind: mkBin8, vBin8: v)

proc Bin16*(v: seq[byte]): Msg =
  assert(len(v) < (1 shl 16))
  Msg(kind: mkBin16, vBin16: v)

proc Bin32*(v: seq[byte]): Msg =
  assert(len(v) < (1 shl 32))
  Msg(kind: mkBin32, vBin32: v)

proc FixExt1*(v: Ext): Msg =
  assert(len(v.data) == 1)
  Msg(kind: mkFixExt1, vFixExt1: v)

proc FixExt2*(v: Ext): Msg =
  assert(len(v.data) == 2)
  Msg(kind: mkFixExt2, vFixExt2: v)

proc FixExt4*(v: Ext): Msg =
  assert(len(v.data) == 4)
  Msg(kind: mkFixExt4, vFixExt4: v)

proc FixExt8*(v: Ext): Msg =
  assert(len(v.data) == 8)
  Msg(kind: mkFixExt8, vFixExt8: v)

proc FixExt16*(v: Ext): Msg =
  assert(len(v.data) == 16)
  Msg(kind: mkFixExt16, vFixExt16: v)

proc Ext8*(v: Ext): Msg =
  assert(len(v.data) < (1 shl 8))
  Msg(kind: mkExt8, vExt8: v)

proc Ext16*(v: Ext): Msg =
  assert(len(v.data) < (1 shl 16))
  Msg(kind: mkExt16, vExt16: v)

proc Ext32*(v: Ext): Msg =
  assert(len(v.data) < (1 shl 32))
  Msg(kind: mkExt32, vExt32: v)

{.pop.}

# ------------------------------------------------------------------------------

#
# Nim Object => Msg Object
#

proc convM(x: bool): Msg =
  assert(false)
  True

proc convM(x: int): Msg =
  if 0 <= x:
    if x < 0b10000000:
      PFixNum(cast[uint8](x.toU8))
    elif x < 0x100:
      UInt8(cast[uint8](x.toU8))
    elif x < 0x10000:
      UInt16(cast[uint16](x.toU16))
    elif x < 0x100000000:
      UInt32(cast[uint32](x.toU32))
    else:
      UInt64(x.uint64)
  else:
    if x >= -32:
      NFixNum(cast[int8](x.toU8))
    elif x >= -128:
      Int8(cast[int8](x.toU8))
    elif x >= -(1 shl 15):
      Int16(cast[int16](x.toU16))
    elif x >= -(1 shl 31):
      Int32(cast[int32](x.toU32))
    else:
      Int64(x.int64)

proc convM(x: float): Msg =
  assert(false)
  Nil # TODO

proc convM(x: string): Msg =
  let sz = len(x)
  if sz < 32:
    FixStr(x)
  elif sz < 0x100:
    Str8(x)
  elif sz < 0x10000:
    Str16(x)
  elif sz < 0x100000000:
    Str32(x)
  else:
    assert(false)
    nil

proc convM(x: seq[byte]): Msg =
  let sz = len(x)
  if sz < 0x100:
    Bin8(x)
  elif sz < 0x10000:
    Bin16(x)
  elif sz < 0x100000000:
    Bin32(x)
  else:
    assert(false)
    nil

proc convM(x: seq[Msg]): Msg =
  let sz = len(x)
  if sz < 16:
    FixArray(x)
  elif sz < 0x10000:
    Array16(x)
  elif sz < 0x100000000:
    Array32(x)
  else:
    assert(false)
    nil

proc convM(x: seq[tuple[key:Msg, val:Msg]]): Msg =
  let sz = len(x)
  if sz < 16:
    FixMap(x)
  elif sz < 0x10000:
    Map16(x)
  elif sz < 0x100000000:
    Map32(x)
  else:
    assert(false)
    nil

proc convM(x: Ext): Msg =
  let sz = len(x.data)
  if sz == 1:
    FixExt1(x)
  elif sz == 2:
    FixExt2(x)
  elif sz == 4:
    FixExt4(x)
  elif sz == 8:
    FixExt8(x)
  elif sz == 16:
    FixExt16(x)
  elif sz < 0x100:
    Ext8(x)
  elif sz < 0x10000:
    Ext16(x)
  else: # FIXME
    Ext32(x)

#
# Wrappable Type Class
#

type Wrappable = concept x
  wrap(x) is Msg

proc wrap*(x: Msg): Msg =
  x

proc wrap*(x: bool): Msg =
  convM(x)

proc wrap*(x: int): Msg =
  ## Given x of int and returns a Msg object that is most compression-effective.
  ## The rule of thumb is use wrap if you don't know in advance what Msg type it
  ## will become, otherwise use specific conversion to Msg which can reduce
  ## overhead to determine the Msg type.
  convM(x)

proc wrap*(x: float): Msg =
  convM(x)

proc wrap*(x: string): Msg =
  convM(x)

proc wrap*(x: seq[byte]): Msg =
  convM(x)

proc wrap*(x: Ext): Msg =
  convM(x)

proc wrap*[T:Wrappable](x: seq[T]): Msg =
  x.map(wrap).convM

proc wrap*[K: Wrappable, V: Wrappable](x: seq[tuple[key: K, val: V]]): Msg =
  x.map(proc (e: tuple[key: K, val: V]): auto = (wrap(e.key), wrap(e.val))).convM

#
# Implicit converter
#

# TODO ただのエイリアスのようなので処分してもいいかも

converter toMsg*(x: Msg): Msg =
  x

converter toMsg*(x: bool): Msg =
  wrap(x)

converter toMsg*(x: int): Msg =
  wrap(x)

converter toMsg*(x: float): Msg =
  wrap(x)

converter toMsg*(x: string): Msg =
  wrap(x)

converter toMsg*(x: seq[byte]): Msg =
  wrap(x)

converter toMsg*(x: Ext): Msg =
  wrap(x)

converter toMsg*[T:Wrappable](x: seq[T]): Msg =
  wrap(x)

converter toMsg*[K: Wrappable, V: Wrappable](x: seq[tuple[key: K, val: V]]): Msg =
  wrap(x)

# ------------------------------------------------------------------------------

#
# Msg Object => Nim Object
#

proc unwrapBool*(x: Msg): bool =
  case x.kind:
  of mkTrue:
    true
  of mkFalse:
    false
  else:
    assert(false)
    false

proc unwrapInt*(x: Msg): int =
  case x.kind:
  of mkPFixNum:
    x.vPFixNum.int
  of mkNFixNum:
    x.vNFixNum.int
  of mkUInt8:
    x.vUInt8.int
  of mkUInt16:
    x.vUInt16.int
  of mkUInt32:
    x.vUInt32.int
  # FIXME
  # と書いてあるのは、32ビットの時におかしいからかも知れない
  of mkUInt64:
    x.vUInt64.int 
  of mkInt8:
    x.vInt8.int
  of mkInt16:
    x.vInt16.int
  of mkInt32:
    x.vInt32.int
  else:
    x.vInt64.int

proc unwrapFloat*(x: Msg): float =
  case x.kind:
  of mkFloat32:
    x.vFloat32.float
  else:
    x.vFloat64.float

proc unwrapStr*(x: Msg): string =
  case x.kind:
  of mkFixStr:
    x.vFixStr
  of mkStr8:
    x.vStr8
  of mkStr16:
    x.vStr16
  of mkStr32:
    x.vStr32
  else:
    assert(false)
    ""

proc unwrapBin*(x: Msg): seq[byte] =
  case x.kind:
  of mkBin8:
    x.vBin8
  of mkBin16:
    x.vBin16
  of mkBin32:
    x.vBin32
  else:
    assert(false)
    @[]

proc unwrapArray*(x: Msg): seq[Msg] =
  case x.kind:
  of mkFixArray:
    x.vFixArray
  of mkArray16:
    x.vArray16
  of mkArray32:
    x.vArray32
  else:
    assert(false)
    @[]
 
proc unwrapMap*(x: Msg): seq[tuple[key:Msg, val:Msg]] =
  case x.kind:
  of mkFixMap:
    x.vFixMap
  of mkMap16:
    x.vMap16
  of mkMap32:
    x.vMap32
  else:
    assert(false)
    @[]

proc unwrapExt*(x: Msg): Ext =
  case x.kind:
  of mkFixExt1:
    x.vFixExt1
  of mkFixExt2:
    x.vFixExt2
  of mkFixExt4:
    x.vFixExt4
  of mkFixExt8:
    x.vFixExt8
  of mkFixExt16:
    x.vFixExt16
  of mkExt8:
    x.vExt8
  of mkExt16:
    x.vExt16
  else:
    x.vExt32 # FIXME

# ------------------------------------------------------------------------------

type PackBuf = ref object
  st: Stream

# Stream automatically extend the writable area
# so we don't need to manipulate the size by hands.
proc ensureMore(buf: PackBuf, addLen: int) =
  discard

proc appendBe8(buf: PackBuf, v: byte) =
  var vv = v
  buf.st.writeData(addr(vv), 1)

proc appendHeader(buf: PackBuf, v: int) =
  buf.appendBe8(cast[byte](v.toU8))

proc appendBe16(buf: PackBuf, v: b16) =
  var vv = v
  var tmp: b16
  bigEndian16(addr(tmp), addr(vv))
  buf.st.writeData(addr(tmp), 2)

proc appendBe32(buf: PackBuf, v: b32) =
  var vv = v
  var tmp: b32
  bigEndian32(addr(tmp), addr(vv))
  buf.st.writeData(addr(tmp), 4)

proc appendBe64(buf: PackBuf, v: b64) =
  var vv = v
  var tmp: b64
  bigEndian64(addr(tmp), addr(vv))
  buf.st.writeData(addr(tmp), 8)

proc appendData(buf: PackBuf, p: pointer, size: int) =
  buf.st.writeData(p, size)

type Packer = ref object
  buf: PackBuf

proc mkPacker(buf: PackBuf): Packer =
  Packer(buf: buf)

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
  of mkFixArray:
    #echo "fixarray"
    let h: int = 0x90 or len(msg.vFixArray)
    buf.ensureMore(1)
    buf.appendHeader(h)
    pc.appendArray(msg.vFixArray)
  of mkArray16:
    #echo "array16"
    let sz = len(msg.vArray16)
    buf.ensureMore(3)
    buf.appendHeader(0xdc)
    buf.appendBe16(cast[b16](sz.toU16))
    pc.appendArray(msg.vArray16)
  of mkArray32:
    #echo "array32"
    let sz = len(msg.vArray32)
    buf.ensureMore(5)
    buf.appendHeader(0xdd)
    buf.appendBe32(cast[b32](sz.toU32))
    pc.appendArray(msg.vArray32)
  of mkFixMap:
    #echo "fixmap"
    let sz = len(msg.vFixMap)
    let h = 0x80 or sz
    buf.ensureMore(1)
    buf.appendHeader(h)
    pc.appendMap(msg.vFixMap)
  of mkMap16:
    #echo "map16"
    let sz = len(msg.vMap16)
    buf.ensureMore(3)
    buf.appendHeader(0xde)
    buf.appendBe16(cast[b16](sz.toU16))
    pc.appendMap(msg.vMap16)
  of mkMap32:
    #echo "map32"
    let sz = len(msg.vMap32)
    buf.ensureMore(5)
    buf.appendHeader(0xdf)
    buf.appendBe32(cast[b32](sz.toU32))
    pc.appendMap(msg.vMap32)
  of mkNil:
    #echo "nil"
    buf.ensureMore(1)
    buf.appendHeader(0xc0)
  of mkFalse:
    #echo "false"
    buf.ensureMore(1)
    buf.appendHeader(0xc2)
  of mkTrue:
    #echo "true"
    buf.ensureMore(1)
    buf.appendHeader(0xc3)
  of mkPFixNum:
    #echo "pfixnum"
    let h: int = 0x7f and msg.vPFixNum.int
    buf.ensureMore(1)
    buf.appendHeader(h)
  of mkNFixNum:
    #echo "nfixnum"
    buf.ensureMore(1)
    buf.appendHeader(msg.vNFixNum.int)
  of mkUInt8:
    #echo "uint8"
    buf.ensureMore(1+1)
    buf.appendHeader(0xcc)
    buf.appendBe8(cast[byte](msg.vUInt8))
  of mkUInt16:
    #echo "uint16"
    buf.ensureMore(1+2)
    buf.appendHeader(0xcd)
    buf.appendBe16(cast[b16](msg.vUInt16))
  of mkUInt32:
    #echo "uint32"
    buf.ensureMore(1+4)
    buf.appendHeader(0xce)
    buf.appendBe32(cast[b32](msg.vUInt32))
  of mkUInt64:
    #echo "uint64"
    buf.ensureMore(1+8)
    buf.appendHeader(0xcf)
    buf.appendBe64(cast[b64](msg.vUInt64))
  of mkInt8:
    #echo "int8"
    buf.ensureMore(1+1)
    buf.appendHeader(0xd0)
    buf.appendBe8(cast[byte](msg.vInt8))
  of mkInt16:
    #echo "int16"
    buf.ensureMore(1+2)
    buf.appendHeader(0xd1)
    buf.appendBe16(cast[b16](msg.vInt16))
  of mkInt32:
    #echo "int32"
    buf.ensureMore(1+4)
    buf.appendHeader(0xd2)
    buf.appendBe32(cast[b32](msg.vInt32))
  of mkInt64:
    #echo "int64"
    buf.ensureMore(1+8)
    buf.appendHeader(0xd3)
    buf.appendBe64(cast[b64](msg.vInt64))
  of mkFloat32:
    #echo "float32"
    buf.ensureMore(1+4)
    buf.appendHeader(0xca)
    buf.appendBe32(cast[b32](msg.vFloat32))
  of mkFloat64:
    #echo "float64"
    buf.ensureMore(1+8)
    buf.appendHeader(0xcb)
    buf.appendBe64(cast[b64](msg.vFloat64))
  of mkFixStr:
    #echo "fixstr"
    let sz: int = len(msg.vFixStr)
    let h = 0xa0 or sz
    buf.ensureMore(1+sz)
    buf.appendHeader(h)
    var m = msg
    buf.appendData(addr(m.vFixStr[0]), sz)
  of mkStr8:
    #echo "str8"
    let sz = len(msg.vStr8)
    buf.ensureMore(1 + sz)
    buf.appendHeader(0xd9)
    buf.appendBe8(cast[byte](sz.toU8))
    var m = msg
    buf.appendData(addr(m.vStr8[0]), sz)
  of mkStr16:
    #echo "str16"
    let sz = len(msg.vStr16)
    buf.ensureMore(3 + sz)
    buf.appendHeader(0xda)
    buf.appendBe16(cast[b16](sz.toU16))
    var m = msg
    buf.appendData(addr(m.vStr16[0]), sz)
  of mkStr32:
    #echo "str32"
    let sz = len(msg.vStr32)
    buf.ensureMore(5 + sz)
    buf.appendHeader(0xdb)
    buf.appendBe32(cast[b32](sz.toU32))
    var m = msg
    buf.appendData(addr(m.vStr32[0]), sz)
  of mkBin8:
    #echo "bin8"
    let sz = len(msg.vBin8)
    buf.ensureMore(1+1+sz)
    buf.appendHeader(0xc4)
    buf.appendBe8(cast[byte](sz.toU8))
    var m = msg
    buf.appendData(addr(m.vBin8[0]), sz)
  of mkBin16:
    #echo "bin16"
    let sz = len(msg.vBin16)
    buf.ensureMore(1+2+sz)
    buf.appendHeader(0xc5)
    buf.appendBe16(cast[b16](sz.toU16))
    var m = msg
    buf.appendData(addr(m.vBin16[0]), sz)
  of mkBin32:
    #echo "bin32"
    let sz = len(msg.vBin32)
    buf.ensureMore(1+4+sz)
    buf.appendHeader(0xc6)
    buf.appendBe32(cast[b32](sz.toU32))
    var m = msg
    buf.appendData(addr(m.vBin32[0]), sz)
  of mkFixExt1:
    #echo "fixext1"
    buf.ensureMore(3)
    buf.appendHeader(0xd4)
    var (a, b) = msg.vFixExt1
    buf.appendBe8(cast[byte](a))
    # var m = msg
    buf.appendData(addr(b[0]), 1)
  of mkFixExt2:
    #echo "fixext2"
    buf.ensureMore(4)
    buf.appendHeader(0xd5)
    var (a, b) = msg.vFixExt2
    buf.appendBe8(cast[byte](a))
    # var m = msg
    buf.appendData(addr(b[0]), 2)
  of mkFixExt4:
    #echo "fixext4"
    buf.ensureMore(6)
    buf.appendHeader(0xd6)
    var (a, b) = msg.vFixExt4
    buf.appendBe8(cast[byte](a))
    # var m = msg
    buf.appendData(addr(b[0]), 4)
  of mkFixExt8:
    #echo "fixext8"
    buf.ensureMore(10)
    buf.appendHeader(0xd7)
    var (a, b) = msg.vFixExt8
    buf.appendBe8(cast[byte](a))
    # var m = msg
    buf.appendData(addr(b[0]), 8)
  of mkFixExt16:
    #echo "fixext16"
    buf.ensureMore(18)
    buf.appendHeader(0xd8)
    var (a, b) = msg.vFixExt16
    buf.appendBe8(cast[byte](a))
    # var m = msg
    buf.appendData(addr(b[0]), 16)
  of mkExt8:
    #echo "ext8"
    var (a, b) = msg.vExt8
    let sz = len(b)
    buf.ensureMore(1+1+1+sz)
    buf.appendHeader(0xc7)
    buf.appendBe8(cast[byte](sz.toU8))
    buf.appendBe8(cast[byte](a))
    # var m = msg
    buf.appendData(addr(b[0]), sz)
  of mkExt16:
    #echo "ext16"
    var (a, b) = msg.vExt16
    let sz = len(b)
    buf.ensureMore(1+2+1+sz)
    buf.appendHeader(0xc8)
    buf.appendBe16(cast[b16](sz.toU16))
    buf.appendBe8(cast[byte](a))
    # var m = msg
    buf.appendData(addr(b[0]), sz)
  of mkExt32:
    #echo "ext32"
    var (a, b) = msg.vExt32
    let sz = len(b)
    buf.ensureMore(1+4+1+sz)
    buf.appendHeader(0xc9)
    buf.appendBe32(cast[b32](sz.toU32))
    buf.appendBe8(cast[byte](a))
    # var m = msg
    buf.appendData(addr(b[0]), sz)

# ------------------------------------------------------------------------------

type UnpackBuf = ref object
  st: Stream

proc popBe8(buf: UnpackBuf): byte =
  cast[byte](buf.st.readInt8)

proc popBe16(buf: UnpackBuf): b16 =
  var v: b16
  var tmp = cast[b16](buf.st.readInt16)
  when cpuEndian == littleEndian:
    swapEndian16(addr(v), addr(tmp))
    v
  else:
    tmp

proc popBe32(buf: UnpackBuf): b32 =
  var v: b32
  var tmp = cast[b32](buf.st.readInt32)
  when cpuEndian == littleEndian:
    swapEndian32(addr(v), addr(tmp))
    v
  else:
    tmp

proc popBe64(buf: UnpackBuf): b64 =
  var v: b64
  var tmp = cast[b64](buf.st.readInt64)
  when cpuEndian == littleEndian:
    swapEndian64(addr(v), addr(tmp))
    v
  else:
    tmp

proc popData(buf: UnpackBuf, p: pointer, size: int) =
  discard buf.st.readData(p, size)

type Unpacker = ref object
  buf: UnpackBuf

proc unpack(upc: Unpacker): Msg

proc popArray(upc: Unpacker, size: int): seq[Msg] =
  result = newSeq[Msg](size)
  for i in 0..(size-1):
    result[i] = upc.unpack
  
proc popMap(upc: Unpacker, size: int): seq[tuple[key:Msg, val:Msg]] =
  result = newSeq[tuple[key:Msg, val:Msg]](size)
  for i in 0..(size-1):
    result[i] = (upc.unpack, upc.unpack)

proc mkUnpacker(buf: UnpackBuf): Unpacker =
  Unpacker(buf: buf)

proc unpack(upc: Unpacker): Msg =
  let buf = upc.buf

  let h = cast[uint8](buf.popBe8)
  #echo h.int

  case h
  of 0x90..0x9f:
    #echo "fixarray"
    let sz = h and 0x0f
    FixArray(upc.popArray(sz.int))
  of 0xdc:
    #echo "array16"
    let sz = cast[uint16](buf.popBe16)
    Array16(upc.popArray(sz.int))
  of 0xdd:
    #echo "array32"
    let sz = cast[uint32](buf.popBe32)
    Array32(upc.popArray(sz.int))
  of 0x80..0x8f:
    #echo "fixmap"
    let sz = h and 0x0f
    FixMap(upc.popMap(sz.int))
  of 0xde:
    #echo "map16"
    let sz = cast[uint16](buf.popBe16)
    Map16(upc.popMap(sz.int))
  of 0xdf:
    #echo "map32"
    let sz = cast[uint32](buf.popBe32)
    Map32(upc.popMap(sz.int))
  of 0xc0:
    #echo "nil"
    Nil
  of 0xc3:
    #echo "true"
    True
  of 0xc2:
    #echo "false"
    False
  of 0x00..0x7f:
    #echo "pfixnum"
    let v = h and 0x7f
    PFixNum(cast[uint8](v))
  of 0xe0..0xff:
    #echo "nfixnum"
    NFixNum(cast[int8](h))
  of 0xcc:
    #echo "uint8"
    UInt8(cast[uint8](buf.popBe8))
  of 0xcd:
    #echo "uint16"
    UInt16(cast[uint16](buf.popBe16))
  of 0xce:
    #echo "uint32"
    UInt32(cast[uint32](buf.popBe32))
  of 0xcf:
    #echo "uint64"
    UInt64(cast[uint64](buf.popBe64))
  of 0xd0:
    #echo "int8"
    Int8(cast[int8](buf.popBe8))
  of 0xd1:
    #echo "int16"
    Int16(cast[int16](buf.popBe16))
  of 0xd2:
    #echo "int32"
    Int32(cast[int32](buf.popBe32))
  of 0xd3:
    #echo "int64"
    Int64(cast[int64](buf.popBe64))
  of 0xca:
    #echo "float32"
    Float32(cast[float32](buf.popBe32))
  of 0xcb:
    #echo "float64"
    Float64(cast[float64](buf.popBe64))
  of 0xa0..0xbf:
    #echo "fixstr"
    let sz: int = h.int and 0x1f
    var s = newString(sz)
    buf.popData(addr(s[0]), sz)
    FixStr(s)
  of 0xd9:
    #echo "str8"
    let sz = cast[uint8](buf.popBe8)
    var s = newString(sz.int)
    buf.popData(addr(s[0]), sz.int)
    Str8(s)
  of 0xda:
    #echo "str16"
    let sz = cast[uint16](buf.popBe16)
    var s = newString(sz.int)
    buf.popData(addr(s[0]), sz.int)
    Str16(s)
  of 0xdb:
    #echo "str32"
    let sz = cast[uint32](buf.popBe32)
    var s = newString(sz.int)
    buf.popData(addr(s[0]), sz.int)
    Str32(s)
  of 0xc4:
    #echo "bin8"
    let sz = cast[uint8](buf.popBe8)
    var d = newSeq[byte](sz.int)
    buf.popData(addr(d[0]), sz.int)
    Bin8(d)
  of 0xc5:
    #echo "bin16"
    let sz = cast[uint16](buf.popBe16)
    var d = newSeq[byte](sz.int)
    buf.popData(addr(d[0]), sz.int)
    Bin16(d)
  of 0xc6:
    #echo "bin32"
    let sz = cast[uint32](buf.popBe32)
    var d = newSeq[byte](sz.int)
    buf.popData(addr(d[0]), sz.int)
    Bin32(d)
  of 0xd4:
    #echo "fixext1"
    let t = cast[uint8](buf.popBe8)
    var d = newSeq[byte](1)
    buf.popData(addr(d[0]), 1)
    FixExt1((t.int, d))
  of 0xd5:
    #echo "fixext2"
    let t = cast[uint8](buf.popBe8)
    var d = newSeq[byte](2)
    buf.popData(addr(d[0]), 2)
    FixExt2((t.int, d))
  of 0xd6:
    #echo "fixext4"
    let t = cast[uint8](buf.popBe8)
    var d = newSeq[byte](4)
    buf.popData(addr(d[0]), 4)
    FixExt4((t.int, d))
  of 0xd7:
    #echo "fixext8"
    let t = cast[uint8](buf.popBe8)
    var d = newSeq[byte](8)
    buf.popData(addr(d[0]), 8)
    FixExt8((t.int, d))
  of 0xd8:
    #echo "fixext16"
    let t = cast[uint8](buf.popBe8)
    var d = newSeq[byte](16)
    buf.popData(addr(d[0]), 16)
    FixExt16((t.int, d))
  of 0xc7:
    #echo "ext8"
    let sz = cast[uint8](buf.popBe8)
    let t = cast[uint8](buf.popBe8)
    var d = newSeq[byte](sz.int)
    buf.popData(addr(d[0]), sz.int)
    Ext8((t.int, d))
  of 0xc8:
    #echo "ext16"
    let sz = cast[uint16](buf.popBe16)
    let t = cast[uint8](buf.popBe8)
    var d = newSeq[byte](sz.int)
    buf.popData(addr(d[0]), sz.int)
    Ext16((t.int, d))
  of 0xc9:
    #echo "ext32"
    let sz = cast[uint32](buf.popBe32)
    let t = cast[uint8](buf.popBe8)
    var d = newSeq[byte](sz.int)
    buf.popData(addr(d[0]), sz.int)
    Ext32((t.int, d))
  else:
    assert(false) # not reachable
    Nil

# ------------------------------------------------------------------------------

proc pack*(st: Stream, msg: Msg) =
  ## Serialize message to streaming byte sequence
  let buf = PackBuf(st: st)
  let pc = mkPacker(buf)
  pc.pack(msg)

proc unpack*(st: Stream): Msg =
  ## Deserialize streaming byte sequence to message
  let buf = UnpackBuf(st: st)
  let upc = mkUnpacker(buf)
  upc.unpack

# ------------------------------------------------------------------------------

proc t*(msg: Msg) =
  # もともと$expr(msg)だった。怪しい
  let before = $msg
  #echo before
  let st = newStringStream()
  assert(st.getPosition == 0)
  st.pack(msg)
  assert(st.getPosition != 0)
  st.setPosition(0)
  let unpacked = st.unpack
  let after = $unpacked
  #echo after
  assert(before == after)

when isMainModule:
  t(FixArray(@[True, False]))
  t(Array16(@[True, False]))
  t(Array32(@[True, False]))
  t(FixMap(@[(Nil,True),(False,UInt16(1))]))
  t(Map16(@[(Nil,True),(False,UInt16(1))]))
  t(Map32(@[(Nil,True),(False,UInt16(1))]))
  t(Nil)
  t(True)
  t(False)
  t(PFixNum(127))
  t(NFixNum(-32))
  t(UInt8(255))
  t(UInt16(10000))
  t(UInt32(10000))
  t(UInt64(10000))
  t(Int8(127))
  t(Int16(10000))
  t(Int32(10000))
  t(Int64(10000))
  t(Float32(0.12345'f32))
  t(Float64(0.78901'f64))
  t(FixStr("akiradeveloper"))
  t(Str8("akiradeveloper"))
  t(Str16("akiradeveloper"))
  t(Str32("akiradeveloper"))
  t(Bin8(@[cast[byte](4),5,6]))
  t(Bin16(@[cast[byte](4),5,6]))
  t(Bin32(@[cast[byte](4),5,6]))
  t(FixExt1((12, @[cast[byte](1)])))
  t(FixExt2((12, @[cast[byte](1), 2])))
  t(FixExt4((12, @[cast[byte](1), 2, 3, 4])))
  t(FixExt8((12, @[cast[byte](1), 2, 3, 4, 5, 6, 7, 8])))
  t(FixExt16((12, @[cast[byte](1), 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16])))
  t(Ext8((12, @[cast[byte](1),2,3])))
  t(Ext16((12, @[cast[byte](1),2,3])))
  t(Ext32((12, @[cast[byte](1),2,3])))

  t(FixArray(@[FixArray(@[True, False]), PFixNum(18)]))
  # t(FixArray(toseq(@[FixArray(toseq(@[True, False])), FixArray(toseq(@[True, False]))])))

  let a: Msg = @[Int8(1),Int32(2)]
  echo a

  let b: Msg = @[@[1], @[2]]
  echo b

  let cc: seq[tuple[key: int, val:int]] = @[(1,1), (2,2)]
  let c: Msg = wrap(cc)
  echo c

  let f: Msg = wrap(@["a", "b"])
  echo f

  let g: Msg = wrap(@[("a", "c"), ("b", "d")])
  echo g

  # let ee: seq[tuple[key: int, val: string]] = @[(1, "a"), (2, "b")]
  # let e: Msg = wrap(ee)
  # echo e

  # let d: Msg = wrap(@[(1,@[1,2]), (3,@[1,2])])
  # echo d
