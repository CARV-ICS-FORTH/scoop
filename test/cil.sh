#!/bin/bash

export CILLY_DONT_COMPILE_AFTER_MERGE=1
export CILLY_DONT_LINK_AFTER_MERGE=1
../cil/bin/cilly -include tpc_s2s.h --bytecode -I./include/ppu -I./include/spu $*
