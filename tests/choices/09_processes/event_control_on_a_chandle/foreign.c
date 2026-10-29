#include <stdlib.h>

void *allocate_token(void) {
  return malloc(1);
}

void release_token(void *token) {
  free(token);
}
