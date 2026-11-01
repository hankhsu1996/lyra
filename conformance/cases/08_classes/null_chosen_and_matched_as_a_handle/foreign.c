#include <stdlib.h>

void *allocate_cell(void) {
  return malloc(sizeof(int));
}

void release_cell(void *handle) {
  free(handle);
}
