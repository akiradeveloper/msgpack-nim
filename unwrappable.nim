import msgpack

type Unwrappable[T] = generic x
  x is Msg
  unwrap(x) is T

proc unwrap(x: Unwrappable[int]): int =
  x.unwrapInt

proc unwrap(x: Unwrappable[string]): string =
  x.unwrapString

proc unwrap[T](x: Unwrappable[seq[T]]): seq[T] =
  x.unwrapArray.map(unwrap)

if isMainModule:
  let m1 = wrap(1)
  echo unwrap(m1)

  let m2 = wrap("a")
  echo unwrap(m2)

  let m3 = wrap(@[1,2])
  echo unwrap(m3)
