filename = ARGV[0] # no ext

tests = File.read(filename).split "\n"
# p tests

contents = """
import msgpack
import times

when isMainModule:
  var t0 = cpuTime()
#{tests.map {|x| "  t(#{x})"}.join("\n")}
  echo \"exectime [sec]: \", cpuTime() - t0
"""

f = File.open("#{filename}.nim", "w")
f.write(contents)
f.close
