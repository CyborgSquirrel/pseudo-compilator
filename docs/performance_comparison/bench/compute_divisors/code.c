#include <stdio.h>

int main() {
  int n;
  scanf("%d", &n);

  for (int k = 0; k < n; ++k) {
    long long a;
    scanf("%lld", &a);

    long long i = 1;
    while (i*i <= a) {
      if (a % i == 0) {
        printf("%lld\n", i);
        if (i != a) {
          printf("%lld\n", a/i);
        }
      }
      i += 1;
    }
  }
}
