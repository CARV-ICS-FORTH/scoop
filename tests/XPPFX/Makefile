INCLUDE=../../headers

SCOOP=../../scoop

all: XPPFX1_trans.c XPPFX_trans.c

XPPFX_trans.c:   test.c
	$(SCOOP) --runtime=XPPFX --out-name=XPPFX_trans --tpcIncludePath=${INCLUDE} -I${INCLUDE} $^ $(EXTRAOPTS)

XPPFX1_trans.c: array1.c
	$(SCOOP) --runtime=XPPFX --out-name=XPPFX1_trans --tpcIncludePath=${INCLUDE} -I${INCLUDE} $^ $(EXTRAOPTS)

clean:
	$(RM) -f *.o XPPFX*_trans.c
