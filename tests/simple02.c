#include <stdio.h>
void foo(int* ignore) { return; }

main() {
  int i, *tmp, *A[10];
  tmp = A[i];
  for(i=0; i < 10; i++) {
  /* TODO: 
   * 0. find i++
   * 1. check i++ increases monotonically
   */

    {
    tmp = A[i];
      /* TODO: 
       * 0. find tmp (task argument).
       * 1. compute tmp reaching definitions (use cil reachingdefs.)
       * 2. look up tmp, assert there's exactly 1 definition
       * 3. deconstruct definition as A[i], get varinfo for A, varinfo for i
       */ 
    }
    {
//#pragma css task input(tmp[size])
  /*
   * TODO: check size < sizeof(A[i])
   * 1. find varinfo (or constant) for size (from task def pragma)
   * 2. find array element type, compute(?) sizeof array element
   * 3. ok, don't compute size. print Warning: please verify that 
   *  <size> < sizeof<A element>.  Let the programmer figure it out.
   */
    foo(tmp);
    }
  }
}
