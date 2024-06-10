#include <stdio.h>
#include <stdlib.h>

int main() {
  int n;
  scanf("%d", &n);
  int *list = malloc(sizeof(int) * n);

  for (int i = 0; i < n; ++i) {
    scanf("%d", &list[i]);
  }

  for (int i = 0; i < n/2; ++i) {
    int tmp = list[i];
    list[i] = list[n-1-i];
    list[n-1-i] = tmp;
  }

  for (int i = 0; i < n; ++i) {
    printf("%d\n", list[i]);
  }

  free(list);
}
