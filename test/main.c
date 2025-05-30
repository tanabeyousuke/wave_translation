#include <stdio.h>
#include <stdlib.h>
int* yoyaku(int length)
{
  int *p;
  p = malloc(sizeof(int) * length);
  return p;
}

void output(int* array, int length)
{
  for(int i = 0; i < length; i++)
    {
      printf("%d\n", array[i]);
    }
}

void kaihou(int* p)
{
  free(p);
}
