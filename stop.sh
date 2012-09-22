#!/bin/sh
kill `ps -ef | awk '/pingpong/{ print $2 }'`