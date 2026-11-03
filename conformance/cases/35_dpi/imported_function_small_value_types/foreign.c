/* The C counterpart of each SystemVerilog type this design names, spelled the
   way Table H.1 fixes it. The generated ABI header declares every prototype,
   so a definition here that disagreed with the design would not compile. */
#include <stdio.h>
#include <stdlib.h>
#include <svdpi.h>

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

double realtime_half(double t) {
  return t / 2.0;
}

/* Each step is taken in `float` and rounded there. */
float shortreal_mix(float a, float b) {
  float square = a * a;
  return square * b;
}

/* An inout `float` is read and written through a pointer, and an output one
   only written (LRM Annex H.8.4, H.8.8). */
void shortreal_scale(float* value, float* tripled) {
  *tripled = *value * 3.0F;
  *value = *value * 2.0F;
}

/* An enumeration declared over `int` is an `int` here (LRM Annex H.7.3); its
   names do not cross. */
void next_colour(int32_t c, int32_t* previous, int32_t* next) {
  *previous = c == 40 ? 3 : 0;
  *next = c == 40 ? 500 : 0;
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
