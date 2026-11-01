#include <stdlib.h>

typedef struct {
  int value;
} cell_t;

void make_cell_into(void **out, int seed) {
  cell_t *cell = malloc(sizeof(cell_t));
  cell->value = seed;
  *out = cell;
}

void add_to_cell(void **through, int amount) {
  ((cell_t *)*through)->value += amount;
}

void clear_handle(void **through) {
  *through = NULL;
}

int read_cell(void *handle) {
  return ((cell_t *)handle)->value;
}

void release_cell(void *handle) {
  free(handle);
}
