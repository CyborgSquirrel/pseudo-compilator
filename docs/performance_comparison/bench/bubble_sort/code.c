#include <stdio.h>
#include <stdlib.h>

int main() {
  int n;
  scanf("%d", &n);

  double *list = malloc(sizeof(double) * n);
  for (int i = 0; i < n; ++i) {
    double what;
    scanf("%lf", &list[i]);
  }

  for (int i = 0; i < n-1; ++i) {
    for (int j = 0; j < n-1-i; ++j) {
      if (list[j] > list[j+1]) {
        int tmp = list[j];
        list[j] = list[j+1];
        list[j+1] = tmp;
      }
    }
  }

  for (int i = 0; i < n; ++i) {
    printf("%lf\n", list[i]);
  }

  free(list);
  
  return 0;
}
