---
layout: golisp
title: "Getting started with SteelSeries/GoLisp"
---

<div class="big-header">
    Getting Started with SteelSeries/GoLisp
</div>

## Go ##

First, you'll need to download and install the Go language for your system. Visit the <a href="https://golang.org/doc/install" target="_blank">Go project's Getting started page</a> and follow the instruction for your platform. At this time we suggest Go 1.4.2.

Once you're finished, typing the command `go version` should result in something like (platform details may be different for you) `go version go1.4.2 darwin/amd64`.

## Git ##

Skip this if you already have Git installed.

If you don't you'll need it to get the GoLisp code.  Point your browser at <a href="https://git-scm.com/downloads" target="_blank">the Git downloads page</a>, download the version for your system and install it.

## SteelSeries/GoLisp ##

Now that you have Git and Go installed and running, you need to clone the SteelSeries/GoLisp GitHub repo. Go to an appropriate place in your system and give the command

    git clone https://github.com/SteelSeries/golisp.git

Once the clone has completed cd into the new `golisp` directory.

You can also visit the repo <a href="https://github.com/SteelSeries/golisp" target="_blank">here</a>.

You now need to set the environment variable GOPATH to the current directory On OSX or Linux you would use the command:

    export GOPATH=`pwd`

If you are on Windows, use:

    GOPATH=%CD%

Now that Go knows where you are, you can install the other packages that SteelSeries/GoLisp depends on:

    go get gopkg.in/check.v1
    go get gopkg.in/fatih/set.v0
    go get github.com/jimsmart/bufrr

## Off and running ##

Now, execute the following commands (switching to backslashes if you are on Windows):

    cd src/github.com/steelseries/golisp
    go run main/golisp.go

You should see the following

    Welcome to GoLisp 1.0
    Copyright 2015 SteelSeries
    Evaluate '(quit)' to exit.
    
    >

You are now at the GoLisp REPL prompt and you can type lisp expressions (on a single line, spanning multiple lines isn't supported yet) and GoLisp will evaluate them and print the result:

    > (map + '(1 2 3) '(4 5 6))
    ==> (5 7 9)
    >

## Next steps ##

To get familiar with SteelSeries/GoLisp have a look at the documents and posts listed [here](documents.html). The language reference can be found [here](language-ref.html).
