#include <stdio.h>
#include <stdlib.h>

int main() {
  int n;
  scanf("%d", &n);

  int *sieve = malloc(sizeof(int) * n);

  for (int i = 0; i < n; ++i) {
    sieve[i] = 1;
  }

  for (int i = 2; i < n; ++i) {
    if (sieve[i] != 0) {
      printf("%d\n", i);
    
      for (int j = i+i; j < n; j += i) {
        sieve[j] = 0;
      }
    }
  }

  free(sieve);
}
