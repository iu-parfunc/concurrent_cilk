#include <stdio.h>

int add(int x, int y)
{
  return x + y;
}

void foo(void)
{
  int x = 7 ;
  int y = 8;
  int z = add(x, y);

  printf("result: %d\n", z);
}

int main() 
{
  foo();
  return 0;
}
