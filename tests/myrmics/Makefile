INCLUDE=../../headers

all: myrmics1_trans.c myrmics_trans.c
   
myrmics_trans.c:   test.c
	scoop --runtime=myrmics --out-name=myrmics_trans --tpcIncludePath=${INCLUDE} -I${INCLUDE} $^ $(EXTRAOPTS)

myrmics1_trans.c: array1.c
	scoop --runtime=myrmics --out-name=myrmics1_trans --tpcIncludePath=${INCLUDE} -I${INCLUDE} $^ $(EXTRAOPTS)

clean:
	rm -f *.o myrmics*_trans.c
