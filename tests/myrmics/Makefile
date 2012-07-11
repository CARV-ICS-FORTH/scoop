INCLUDE=../../headers

FLAGS=

all: test_trans.c array1_trans.c
   
%_trans.c: %.c
	scoop --runtime=myrmics --myrmics-table-name=_sys_task_table --out-name=$*_trans --tpcIncludePath=${INCLUDE} -I${INCLUDE} $^ $(EXTRAOPTS)

clean:
	rm -f *.o *_trans.c
