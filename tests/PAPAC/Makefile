INCLUDE=../../headers

all: PAPAC1_trans.c PAPAC_trans.c
   
PAPAC_trans.c:   test.c
	scoop --runtime=PAPAC --out-name=PAPAC_trans --tpcIncludePath=${INCLUDE} -I${INCLUDE} $^ $(EXTRAOPTS)

PAPAC1_trans.c: array1.c
	scoop --runtime=PAPAC --out-name=PAPAC1_trans --tpcIncludePath=${INCLUDE} -I${INCLUDE} $^ $(EXTRAOPTS)

clean:
	rm -f *.o PAPAC*_trans.c
