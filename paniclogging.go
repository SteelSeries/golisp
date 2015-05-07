package golisp

import (
	"fmt"
	"runtime"
	"strings"
	"time"
)

func CallWithPanicProtection(f func(), prefix string, shouldTerminate bool) {
	defer func() {
		if recovered := recover(); recovered != nil {
			stackBuf := make([]byte, 10000)
			stackBuf = stackBuf[:runtime.Stack(stackBuf, false)]
			stack := strings.Split(string(stackBuf), "\n")
			for i := 0; i < 7; i++ {
				fmt.Println(stack[i])
			}
			//fmt.Println("numBytes from stacktrace: ", numBytes)

			if shouldTerminate {
				// Give the log4go log time to flush
				time.Sleep(time.Second * 3)

				// Re-panic if this is a high-level enough problem
				panic(recovered)
			}
		}
	}()

	f()
}
