#!/bin/bash

## head -3 takes the first three lines from the arduino (the first two
## maybe don't have a full temperature reading).

## tr -cd command removes non-printable characters
## http://alvinalexander.com/blog/post/linux-unix/how-remove-non-printable-ascii-characters-file-unix

## grep -v removes empty lines

## tail -1 takes the last line.

head -3 /dev/ttyACM0|tr -cd '\11\12\15\40-\176'|grep -v ^$|tail -1
