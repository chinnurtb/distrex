package main

import (
	"flag"
	"fmt"
	"net"
	"time"
)

func heartbeat(cxn net.Conn, what string) chan bool {
	ch := make(chan bool)
	go func() {
		for {
			select {
			case <-ch:
				fmt.Println("Unlocking...")
				cxn.Write([]byte(fmt.Sprintf("UNLOCK%s", what)))
				return
			default:
				fmt.Println("Heartbeat...")
				<-time.After(time.Second * 5)
				cxn.Write([]byte(fmt.Sprintf("BEAT%s", what)))
			}
		}
	}()
	return ch
}

func main() {
	var record = flag.String("record", "", "The record you want to lock")
	flag.Parse()
	if *record == "" {
		fmt.Println("A record to lock is required!")
		return
	}
	cxn, err := net.Dial("udp", "127.0.0.1:8789")
	if err != nil {
		fmt.Println(err)
		return
	}

	cxn.Write([]byte(fmt.Sprintf("LOCK%s", *record)))
	b := make([]byte, 1024)
	cxn.Read(b)
	var ch chan bool
	if string(b)[:2] == "ok" {
		ch = heartbeat(cxn, *record)
	} else {
		fmt.Println("Didn't lock!")
		fmt.Println(len(string(b)))
	}
	<-time.After(time.Minute)
	ch <- true
}
