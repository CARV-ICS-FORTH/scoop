#!/bin/bash

export CILLY_DONT_COMPILE_AFTER_MERGE=1
export CILLY_DONT_LINK_AFTER_MERGE=1
../cil/bin/cilly -D__powerpc64__=1 --queue-size=16U --commPrintLn --bytecode --merge $*
