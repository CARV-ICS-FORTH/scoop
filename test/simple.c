void task1(int *i) {
  *i++;
}

int main() {
  int foo;
  int *arg = &foo;
  int sz = sizeof(foo);

  #pragma tpc (arg(inout, sz))
  task1(arg);
}
