// +build linux darwin

// sniped from https://github.com/rocaltair/goreadline
package golisp

/*
#include <stdio.h>
#include <stdlib.h>
#include <readline/readline.h>
#include <readline/history.h>
#cgo LDFLAGS: -lreadline
*/
import "C"
import "unsafe"

func ReadLine(prompt *string) *string {
	var cPrompt *C.char
	if prompt != nil {
		cPrompt = C.CString(*prompt)
	}
	cLine := C.readline(cPrompt)
	if cPrompt != nil {
		C.free(unsafe.Pointer(cPrompt))
	}
	if cLine == nil {
		return nil
	}

	line := C.GoString(cLine)
	C.free(unsafe.Pointer(cLine))
	return &line
}

func AddHistory(line string) {
	cLine := C.CString(line)
	C.add_history(cLine)
	C.free(unsafe.Pointer(cLine))
}

func ClearHistory() {
	C.clear_history()
}

func WriteHistoryToFile(fileName string) {
	cFileName := C.CString(fileName)
	C.write_history(cFileName)
	C.free(unsafe.Pointer(cFileName))
}

func LoadHistoryFromFile(fileName string) {
	cFileName := C.CString(fileName)
	C.read_history(cFileName)
	C.free(unsafe.Pointer(cFileName))
}

func TruncateHistoryFile(fileName string, left int) {
	cFileName := C.CString(fileName)
	cLeft := C.int(left)
	C.history_truncate_file(cFileName, cLeft)
	C.free(unsafe.Pointer(cFileName))
}
