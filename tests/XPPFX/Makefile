INCLUDE=../

all: test.c
	scoop --runtime=XPPFX --out-name=XPPFX_trans --tpcIncludePath=${INCLUDE} -I${INCLUDE} $^ $(EXTRAOPTS)

clean:
	rm -f *.o XPPFX_trans*
