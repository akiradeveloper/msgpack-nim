filename = ARGV[0] # no ext

tests = File.read(filename).split "\n"
# p tests

contents = """
import msgpack

when isMainModule:
#{tests.map {|x| "  t(#{x})"}.join("\n")}
"""

f = File.open("#{filename}.nim", "w")
f.write(contents)
f.close
