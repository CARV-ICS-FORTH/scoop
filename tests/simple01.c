#include <stdio.h>

void foo() {
  return;
}

main() {
#pragma css task
  foo();
  return 0;
}
