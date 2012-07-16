INCLUDE=../../headers

FLAGS=

all: test_trans.c array1_trans.c
   
%_trans.c: %.c
	@scoop --runtime=myrmics --out-name=$*_trans --tpcIncludePath=${INCLUDE} -I${INCLUDE} $^ $(EXTRAOPTS)

clean:
	rm -f *.o *_trans.c
