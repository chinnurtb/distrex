package main

import (
	"errors"
	"flag"
	"fmt"
	"net"
	"time"
)

type Distrex struct {
	resource string
	cxn      *net.Conn
	ch       chan bool
}

func New(remote, resource string) (*Distrex, error) {
	cxn, err := net.Dial("udp", "127.0.0.1:8789")
	if err != nil {
		return nil, err
	}
	return &Distrex{
		cxn:      &cxn,
		resource: resource,
	}, nil
}

func (d *Distrex) Lock() error {
	(*d.cxn).Write([]byte(fmt.Sprintf("LOCK%s", d.resource)))
	b := make([]byte, 1024)
	numbytes, err := (*d.cxn).Read(b)
	if err != nil {
		return err
	}
	if string(b)[:numbytes] == "ok" {
		d.ch = heartbeat(d.cxn, d.resource)
		return err
	}
	return errors.New("Couldn't lock the resource.")
}

func (d *Distrex) Unlock() {
	d.ch <- true
}

func (d *Distrex) WaitLock() {
	for !d.Check() {
		<-time.After(time.Second * 5)
	}
	d.Lock()
}

func (d *Distrex) Check() bool {
	(*d.cxn).Write([]byte(fmt.Sprintf("CHECK%s", d.resource)))
	b := make([]byte, 1024)
	numbytes, err := (*d.cxn).Read(b)
	if err != nil {
		return false
	}

	return string(b)[:numbytes] == "free"
}

func heartbeat(cxn *net.Conn, what string) chan bool {
	ch := make(chan bool)
	go func() {
		for {
			select {
			case <-ch:
				fmt.Println("Unlocking...")
				(*cxn).Write([]byte(fmt.Sprintf("UNLOCK%s", what)))
				return
			default:
				fmt.Println("Heartbeat...")
				<-time.After(time.Second * 5)
				(*cxn).Write([]byte(fmt.Sprintf("BEAT%s", what)))
			}
		}
	}()
	return ch
}

func WithResource(d *Distrex) {
	if err := d.Lock(); err != nil {
		d.WaitLock()
	}
	defer d.Unlock()

	<-time.After(time.Second * 20)

}

func main() {
	var record = flag.String("record", "", "The record you want to lock")
	flag.Parse()
	if *record == "" {
		fmt.Println("A record to lock is required!")
		return
	}
	Lock, err := New("127.0.0.1:8797", *record)
	if err != nil {
		fmt.Println(err)
		return
	}
	WithResource(Lock)
	<-time.After(time.Second * 3)
}
