#!/bin/bash

export CILLY_DONT_COMPILE_AFTER_MERGE=1
export CILLY_DONT_LINK_AFTER_MERGE=1
../cil/bin/cilly -I./include/ppu/ -D__powerpc64__=1 --arch=cell --queue-size=16 --commPrintLn --bytecode --merge $*
