#include <stdlib.h>

int foo(int *arg1) {
	return 1;
}

int goo(int *arg2) {
	return 1;
}

int main(void) {
	int one=1, two=2;
	int a[30], b[10];
	int *arg1, *arg2;
	int arg_size = sizeof(int);
	arg1 = (int*)malloc(arg_size);
	arg2 = (int*)malloc(arg_size);
	//arg1 = &one;
	//arg2 = &two;
	#pragma tpc(arg1(inout, arg_size))
	foo(arg1);
	#pragma tpc(arg2(inout, arg_size))
	goo(arg2);
}
