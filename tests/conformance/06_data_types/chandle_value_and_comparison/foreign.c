#include <stdlib.h>

typedef struct {
  int value;
} cell_t;

void *allocate_cell(int seed) {
  cell_t *cell = malloc(sizeof(cell_t));
  cell->value = seed;
  return cell;
}

int read_cell(void *handle) {
  return ((cell_t *)handle)->value;
}

void release_cell(void *handle) {
  free(handle);
}
