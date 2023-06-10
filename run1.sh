#!/bin/bash
set -e -x

tinygo build -o ./build/tinywhistle ./cmd/whistle/
./build/tinywhistle ./examples/datalog.lisp
