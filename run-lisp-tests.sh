#!/bin/bash -x

set +x

go run src/github.com/steelseries/golisp/main/golisp.go -t tests/*.lsp
