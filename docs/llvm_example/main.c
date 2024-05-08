#include <stdio.h>

int main(int argc, char **argv) {
  int a, b;
  scanf("%d %d", &a, &b);

  int c = a+b;
  printf("%d + %d = %d", a, b, c);
  
  return 0;
}
