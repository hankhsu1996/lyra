#include <stdint.h>

typedef struct {
  uint32_t aval;
  uint32_t bval;
} svLogicVecVal;

/* Package export: integer (4-state 32-bit) return.
   Indirect return convention: void wrapper(svLogicVecVal* result). */
extern void get_integer_42(svLogicVecVal* result);

int call_get_integer_42(void) {
  svLogicVecVal result;
  get_integer_42(&result);
  return (result.aval == 42 && result.bval == 0) ? 1 : 0;
}
