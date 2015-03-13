{.experimental.}

import msgpack

type Wrappable = generic x
  let x: Msg = wrap(x)

proc wrap(x: int): Msg =
  toMsg(x)

proc wrap[T:Wrappable](x: seq[T]): Msg =
  x.map(wrap).toMsg

let x: Msg = wrap(@[1,2])
echo x
