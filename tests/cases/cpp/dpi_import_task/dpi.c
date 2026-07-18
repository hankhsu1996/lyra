int set_pair(int a, int* b, int* c) {
  *b = a * 2;
  *c = a + 1;
  return 0;
}

int accumulate(int delta, int* total) {
  *total = *total + delta;
  return 0;
}

static int saved;

int remember(int x) {
  saved = x;
  return 0;
}

int recall(int* y) {
  *y = saved;
  return 0;
}
