#!/bin/sh
java -jar target/pingpong-1.0.0-SNAPSHOT-standalone.jar mysema $1 $2 > game.log 2>&1 &
