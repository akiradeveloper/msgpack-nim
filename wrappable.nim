{.experimental.}

import msgpack

type Wrappable = generic x
  let x: Msg = wrap(x)

proc wrap(x: bool): Msg =
  toMsg(x)

proc wrap(x: int): Msg =
  toMsg(x)

proc wrap(x: float): Msg =
  toMsg(x)

proc wrap(x: string): Msg =
  toMsg(x)

proc wrap(x: seq[byte]): Msg =
  toMsg(x)

proc wrap(x: Ext): Msg =
  toMsg(x)

proc wrap[T:Wrappable](x: seq[T]): Msg =
  x.map(wrap).toMsg

proc wrap[K: Wrappable, V: Wrappable](x: seq[tuple[key: K, val: V]]): Msg =
  x.map(proc (e: tuple[key: K, val: V]): auto = (wrap(e.key), wrap(e.val))).toMsg

# doesn't compile
# converter toMsg[T:Wrappable](x: T): Msg =
#   wrap(x)

when isMainModule:
  let x:Msg = wrap(@[1,2])
  echo x

  let y:Msg = wrap(@[(1,2), (3,4)])
  echo y

  let z: Msg = wrap(@[@[1], @[2]])
  echo z
