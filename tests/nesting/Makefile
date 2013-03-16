INCLUDE=../../headers

all: nesting1_trans.c nesting_trans.c
   
nesting_trans.c:   test.c
	scoop --runtime=nesting --out-name=nesting_trans --tpcIncludePath=${INCLUDE} -I${INCLUDE} $^ $(EXTRAOPTS)

nesting1_trans.c: array1.c
	scoop --runtime=nesting --out-name=nesting1_trans --tpcIncludePath=${INCLUDE} -I${INCLUDE} $^ $(EXTRAOPTS)

clean:
	rm -f *.o nesting*_trans.c
