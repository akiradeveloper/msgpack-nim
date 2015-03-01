# msgpack-nim

A MessagePack binding for Nim

[![Build Status](https://travis-ci.org/akiradeveloper/msgpack-nim.svg?branch=master)](https://travis-ci.org/akiradeveloper/msgpack-nim)

https://rawgit.com/akiradeveloper/msgpack-nim/master/msgpack.html

msgpack-nim currently provides only the basic functionality.
Please see what's listed in Todo section. Compared to other language bindings, it's well-tested by
1000 auto-generated test cases by Haskell QuickCheck, which always runs
on every commit to Github repository. Please try `make quickcheck` on your local machine
to see what's happening. Have a nice packing!

![Overview](https://rawgit.com/akiradeveloper/msgpack-nim/master/overview.svg)

### Progress

Not yet beta but will be soon.

Checklist:

* ~~fixarray~~  
* ~~array 16~~  
* ~~array 32~~  
* ~~fixmap~~  
* ~~map 16~~  
* ~~map 32~~  
* ~~nil~~  
* ~~true~~  
* ~~false~~  
* ~~positive fix int~~  
* ~~negative fixint~~  
* ~~uint 8~~  
* ~~uint 16~~  
* ~~uint 32~~  
* ~~uint 64~~  
* ~~int8~~  
* ~~int16~~  
* ~~int32~~  
* ~~int64~~  
* ~~float 32~~  
* ~~float 64~~  
* ~~fixstr~~  
* ~~str 8~~  
* ~~str 16~~  
* ~~str 32~~  
* ~~bin 8~~  
* ~~bin 16~~  
* ~~bin 32~~  
* ~~fixext 1~~  
* fixext 2  
* fixext 4  
* fixext 8  
* fixext 16  
* ext 8  
* ext 16  
* ~~ext 32~~  

### Todo

* Implement Messagepack-RPC  
* Implement nim object to/from msg object translation  
* Performance evaluation  
* Talk with offical Ruby implementation  
* Don't repeat yourself: The code now has too much duplications. Using templates?  

### Author

Akira Hayakawa (ruby.wktk@gmail.com)
