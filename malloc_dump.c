// gcc -shared -o malloc_dump.so malloc_dump.c -fPIC
#include <stdio.h>
static int mallocs = 0;
void sayit() { fprintf(stderr, "%d mallocs\n", mallocs); }
int malloc(unsigned long size) {
  if (!mallocs++) atexit(sayit);
  return __libc_malloc(size);
}
