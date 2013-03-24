#!/bin/bash

protoc \
	--plugin=~/.asdf/systems/protobuf/protoc/lisp/protoc-gen-lisp \
	--proto_path=./ \
	--lisp_out=./ \
	./protocol.proto

