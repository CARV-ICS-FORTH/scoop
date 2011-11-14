INCLUDE=../

all: XPPFX1_trans XPPFX_trans
   
XPPFX_trans:   test.c
	scoop --runtime=XPPFX --out-name=XPPFX_trans --tpcIncludePath=${INCLUDE} -I${INCLUDE} $^ $(EXTRAOPTS)

XPPFX1_trans: array1.c
	scoop --runtime=XPPFX --out-name=XPPFX1_trans --tpcIncludePath=${INCLUDE} -I${INCLUDE} $^ $(EXTRAOPTS)

clean:
	rm -f *.o XPPFX_trans*
