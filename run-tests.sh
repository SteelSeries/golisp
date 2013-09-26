#!/bin/bash -x

set +x

TEST_DIRS=$(find src/github.com/steelseries/golisp -name *test.go | xargs -n 1 dirname | sort -u)


for d in $TEST_DIRS
do
    TEST_PKG="${d/src\//}/"
    go test -i $TEST_PKG
    go test $TEST_PKG
    if [ $? -ne "0" ]; then
        echo -e "\033[01;31m go test $TEST_PKG \033[00m"
        exit -1
    fi

done

