#!/bin/bash 

NRUNS="10"

for i in $NRUNS ; do
	./fixedgrid 2>&1 | tee Output/run.$i
done
