test:
	nim c msgpack
	./msgpack

quicktest:
	ghc GenTests.hs
	./GenTests > test
	ruby gentest.rb test
	nim c test
	./test
