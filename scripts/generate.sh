#!/bin/bash

# makes protobuf generation work
export PATH=$PATH:~/.asdf/systems/protobuf/protoc/lisp/:~/.asdf/systems/protobuf/protoc/bin/
ABSPATH=`pwd -P`

# generate the set of lisp classes from the protbuf file. once this is complete,
# lisp classes/file can be used *without* compiling the protobuf again, which is
# great because we can distribute the lisp file without having to worry is a
# user can install/compile the protoc-gen-lisp util.
protoc \
	--proto_path=$ABSPATH \
	--lisp_out=. \
	$ABSPATH/ql2.proto

# use a custom package name (cl-rethinkdb-proto) ...avoids any potential name
# clashes.
sed -i 's|#:protocol-buffer|#:cl-rethinkdb-proto|' ql2.lisp
sed -i 's|protocol-buffer::|cl-rethinkdb-proto::|' ql2.lisp
# on sense in having inconsistencies
sed -i 's|ql2\.lisp|protocol.lisp|' ql2.lisp
# move it to its proper location
mv ql2.lisp ../protocol.lisp

