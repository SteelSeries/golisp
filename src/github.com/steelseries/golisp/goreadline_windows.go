package golisp

import (
    "bufio"
    "fmt"
    "os"
)

func ReadLine(prompt *string) *string {
    if prompt != nil {
        fmt.Printf("%s", *prompt)
    }

    s := bufio.NewScanner(os.Stdin)
    s.Scan()

    result := s.Text()
    return &result
}

func AddHistory(line string) {
    // TODO
}

func ClearHistory() {
    // TODO
}

func WriteHistoryToFile(fileName string) {
    // TODO
}

func LoadHistoryFromFile(fileName string) {
    // TODO
}

func TruncateHistoryFile(fileName string, left int) {
    // TODO
}
