
#include <stdio.h>
#include <stdlib.h>

void init_array(int *a, int start, int size){
  int i=0;
  printf("%s | a=%p start=%d size=%d\n",__FUNCTION__,a,start,size);
  for(i=0 ; i<size ; i++){
    a[i] = start+i;
  }
  return;
}

void add_array(int *a, int mul, int size){
  int i=0;
  printf("%s | a=%p mul=%d size=%d\n",__FUNCTION__,a,mul,size);
  for(i=0 ; i<size ; i++){
    a[i] += mul*1000;
  }
  return;
}

void print_array(int *a, int size){
  int i=0;
  printf("%s | a=%p size=%d\n",__FUNCTION__,a,size);
  for(i=0 ; i<size ; i++){
    printf("a[%04d]=%d\n",i,a[i]);
  }
  return;
}

int main (int argc, char * argv){

  int * arr=NULL;
  int i=0;
  int n=1024;
  int k=4;
  int m = n/k;

//  #pragma css start

  //arr=(int *)malloc(1024*sizeof(int));
  //arr=(int *)tpc_malloc(n*sizeof(int));
//#pragma css malloc
  arr=(int *)malloc(n*sizeof(int));

/*
  printf("arr %p\n",arr);
  printf("n %p\n",&n);
  #pragma css task in(arr[n], n)
  print_array(arr,n);
*/
  // initialization
  for (i=0 ; i<k ; i++){
    int j = i*m;
    int *arg1 = &arr[i*m];
    printf("arr %p\n",arr);
    printf("m %p\n",&m);
    printf("j %p\n",&j);
    printf("arg1 %p\n",arg1);
    #pragma myrmics task out(arg1) in(j,m)
    init_array(arg1, j, m);
  }

  for(i=0 ; i<k ; i++){
    int j=i+1;
    int *arg1 = &arr[i*m];
    printf("arr %p\n",arr);
    printf("m %p\n",&m);
    printf("j %p\n",&j);
    printf("arg1 %p\n",arg1);
    #pragma myrmics task inout(arg1) in(j,m)
    add_array(arg1, j, m);
  }
  
  printf("arr %p\n",arr);
  printf("n %p\n",&n);
  #pragma myrmics task in(arr[n], n)
  print_array(arr,n);

//#pragma css free
  free(arr);

  //#pragma css finish

  return 0;
}