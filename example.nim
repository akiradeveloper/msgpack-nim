import msgpack
import streams

# You can use any stream subclasses to serialize/deserialize
# messages. e.g. FileStream
let st: Stream = newStringStream()

assert(st.getPosition == 0)

# Type checking protects you from making trivial mistakes.
# Now we pack {"a":[5,-3], "b":[1,2,3]} but more complex
# combination of any Msg types is allowed.
#
# In xs we can mix specific conversion (PFixNum) and generic
# conversion (unwrap).
let xs: Msg = wrap(@[PFixNum(5), (-3).wrap])
let ys: Msg = wrap(@[("a".wrap, xs.wrap), ("b".wrap, @[1, 2, 3].wrap)])
st.pack(ys.wrap) # Serialize!

# We need to reset the cursor to the beginning of the target
# byte sequence.
st.setPosition(0)

let msg = st.unpack # Deserialize!

# output:
# a
# 5
# -3
# b
# 1
# 2
# 3
for e in msg.unwrapMap:
  echo e.key.unwrapStr
  for e in e.val.unwrapArray:
    echo e.unwrapInt
