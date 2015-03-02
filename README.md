# msgpack-nim

A MessagePack binding for Nim

[![Build Status](https://travis-ci.org/akiradeveloper/msgpack-nim.svg?branch=master)](https://travis-ci.org/akiradeveloper/msgpack-nim)

API: https://rawgit.com/akiradeveloper/msgpack-nim/master/msgpack.html

msgpack-nim currently provides only the basic functionality.
Please see what's listed in Todo section. Compared to other language bindings, it's well-tested by
1000 auto-generated test cases by Haskell QuickCheck, which always runs
on every commit to Github repository. Please try `make quickcheck` on your local machine
to see what happens (It will take a bit while. Be patient). Have a nice packing!

![Overview](https://rawgit.com/akiradeveloper/msgpack-nim/master/overview.svg)

### Todo

* Implement Messagepack-RPC  
* Implement nim object to/from Msg object translation (but I am a bit skeptical about this idea of
  mapping nim object to Msg object)  
* Performance evaluation  
* Talk with offical Ruby implementation  
* Don't repeat yourself: The code now has too much duplications. Using templates?  

### Author

Akira Hayakawa (ruby.wktk@gmail.com)
