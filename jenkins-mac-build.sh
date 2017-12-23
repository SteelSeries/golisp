#!/bin/bash
set -e
export CD=(`pwd`)

export GOPATH=$CD
export APP_DATA_PATH=$CD

install () {
  echo "building the go app...."
  ruby get_go_deps.rb
  (go clean -i)
}

test () {
  echo "Starting tests execution...."

  #files=`grep -r  'debug #t' deviceSpecifications/*.device`
  #if [ $? -eq 0 ]; then
  #  echo "*****************************************************************************************"
  #  echo "HeHe!! Found Device files with DEBUG statements. Did you get warning when running tests??"
  #  echo "Files: $files"
  #  echo "*****************************************************************************************"
  #  exit -1
  #fi

  mkdir -p reports

  go test -i -ldflags=-linkmode=external github.com/SteelSeries/golisp

  TEST_DIRS=$(find src/github.com/SteelSeries/golisp -name *test.go | xargs -n 1 dirname | sort -u)

  for d in $TEST_DIRS
  do
        TEST_PKG="${d/src\//}"
        go test -i $TEST_PKG
        go test $TEST_PKG
  done
}


install
test
