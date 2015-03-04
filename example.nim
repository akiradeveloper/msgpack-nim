import msgpack
import streams

# You can use any stream subclasses to serialize/deserialize
# messages. e.g. FileStream
let st: Stream = newStringStream()

assert(st.getPosition == 0)

# Type checking protects you from making trivial mistakes.
# Now we pack {"ints":[5,-3]} but more complex combination of
# any Msg types is allowed.
let xs = @[PFixNum(5), NFixNum(-3)]
let ys = @[(key: FixStr("ints"), val: FixArray(xs.toIterable))]
st.pack(FixMap(ys.toIterable)) # Serialize!

# We need to reset the cursor to the beginning of the target
# byte sequence.
st.setPosition(0)

let msg = st.unpack # Deserialize!

for k, v in msg.vFixMap.iter():
  echo k.vFixStr # emits "ints"
  for e in v.vFixArray.iter():
    # emits 5 and then -3
    case e.kind:
    of mkPFixNum:
      echo e.vPFixNum
    of mkNFixNum:
      echo e.vNFixNum
    else:
      assert(false)
