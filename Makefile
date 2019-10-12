test:
	nim c msgpack
	./msgpack

quickcheck:
	stack build
	stack exec GenTests-exe > test
	ruby gentest.rb test
	nim c -d:release test
	./test

rpctest:
	nim c rpc
	./rpc server &
	./rpc client
