test:
	nim c msgpack
	./msgpack

quickcheck:
	ghc GenTests.hs
	./GenTests > test
	ruby gentest.rb test
	nim c -d:release test
	./test
