#!/bin/bash
set -e -x

rm -rf coverdata/ && mkdir coverdata
go build -cover -o ./build/whistle ./cmd/whistle/
./build/whistle 
go tool covdata textfmt -i=coverdata -o profile.txt
