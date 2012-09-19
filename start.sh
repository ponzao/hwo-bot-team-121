#!/bin/sh
java -jar pingpong-1.0.0-SNAPSHOT-standalone.jar $1 $2 $3 > game.log 2>&1 &
