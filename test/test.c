#include <stdio.h>

int add(int a, int b){
    return a+b;
}

int main(){
	int a=2, b=4;
    int arr[15];

    for (a=0; a<30; ++a) {
      #pragma css task input(arr[15*sizeof(int)], b[sizeof(int)])
      add(arr[3],b);
    }

#pragma css wait on(me)

	return 0;
}
