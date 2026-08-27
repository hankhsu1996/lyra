/* The C counterpart of each SystemVerilog type this design names, spelled the
   way Table H.1 fixes it. The generated ABI header declares every prototype,
   so a definition here that disagreed with the design would not compile. */
#include <stdio.h>
#include <stdlib.h>

#include "dpi.h"

int8_t byte_mix(int8_t a, int8_t b) {
  return (int8_t)((a * 3) + b);
}

int16_t shortint_mix(int16_t a, int16_t b) {
  return (int16_t)((a * 100) + b);
}

int32_t int_mix(int32_t a, int32_t b) {
  return (a * 1000) + b;
}

int64_t longint_mix(int64_t a, int64_t b) {
  return (a * 1000000000) + b;
}

double real_mix(double a, double b) {
  return (a * 8.0) + b;
}

svBit bit_xor(svBit a, svBit b) {
  return (svBit)(a ^ b);
}

/* The four values a scalar logic carries, each mapped to a different one, so
   no argument shares an answer with another (LRM Annex H.10.1.1). */
svLogic rotate_logic(svLogic v) {
  if (v == sv_0) {
    return sv_1;
  }
  if (v == sv_1) {
    return sv_z;
  }
  if (v == sv_z) {
    return sv_x;
  }
  return sv_0;
}

const char* join_text(const char* a, const char* b) {
  static char buffer[64];
  snprintf(buffer, sizeof buffer, "%s|%s", a, b);
  return buffer;
}

/* A chandle is an opaque pointer to memory this side owns; the design carries
   it and hands it back untouched (LRM Table H.1, 35.5.1.4). */
void* make_cell(int32_t seed) {
  int32_t* cell = malloc(sizeof *cell);
  *cell = (seed * 7) + 1;
  return cell;
}

int32_t read_cell(void* cell) {
  return *(int32_t*)cell;
}

void free_cell(void* cell) {
  free(cell);
}
