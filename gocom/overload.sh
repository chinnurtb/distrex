#! /bin/bash

COUNT=$1

while [ $COUNT -gt 0 ]
do
    go run main.go -record $i &
    let COUNT=COUNT-1
done


