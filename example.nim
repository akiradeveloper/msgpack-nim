import msgpack
import streams

# You can use any stream subclasses to serialize/deserialize
# messages. e.g. FileStream
let st: Stream = newStringStream()

assert(st.getPosition == 0)

# Type checking protects you from making trivial mistakes.
# Now we pack {"ints":[5,-3]} but more complex combination of
# any Msg types is allowed.
let xs = @[5.toMsg, (-3).toMsg]
let ys = @[(key: "ints".toMsg, val: xs.toMsg)]
st.pack(FixMap(ys)) # Serialize!

# We need to reset the cursor to the beginning of the target
# byte sequence.
st.setPosition(0)

let msg = st.unpack # Deserialize!

for e in msg.unwrapMap:
  echo e.key.unwrapStr # emits "ints"
  for e in e.val.unwrapArray:
    # emits 5 and then -3
    echo e.unwrapInt
    
