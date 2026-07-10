#include <stdlib.h>

typedef struct {
  int v;
} Obj;

void* make_obj(int seed) {
  Obj* o = malloc(sizeof(Obj));
  o->v = seed;
  return o;
}

int read_obj(void* h) {
  return ((Obj*)h)->v;
}

void alloc_obj(void** h, int seed) {
  Obj* o = malloc(sizeof(Obj));
  o->v = seed;
  *h = o;
}

// Reads the incoming handle and writes a different one back, so the copy-in and
// the copy-back halves of an inout chandle are both exercised.
void bump_obj(void** h) {
  Obj* old = (Obj*)*h;
  Obj* fresh = malloc(sizeof(Obj));
  fresh->v = old->v + 1;
  free(old);
  *h = fresh;
}

void free_obj(void* h) {
  free(h);
}
