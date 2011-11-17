INCLUDE=../

all: XPPFX1_trans.c XPPFX_trans.c
   
XPPFX_trans.c:   test.c
	scoop --runtime=XPPFX --out-name=XPPFX_trans --tpcIncludePath=${INCLUDE} -I${INCLUDE} $^ $(EXTRAOPTS)

XPPFX1_trans.c: array1.c
	scoop --runtime=XPPFX --out-name=XPPFX1_trans --tpcIncludePath=${INCLUDE} -I${INCLUDE} $^ $(EXTRAOPTS)

clean:
	rm -f *.o XPPFX_trans*
