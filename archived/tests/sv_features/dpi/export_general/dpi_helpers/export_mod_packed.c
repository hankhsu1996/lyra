#include <stdint.h>

typedef struct {
  uint32_t aval;
  uint32_t bval;
} svLogicVecVal;

typedef void* svScope;

extern svScope svGetScopeFromName(const char* name);
extern svScope svSetScope(svScope scope);

extern int read_logic8_mod_export(const svLogicVecVal* v);

int call_read_logic8_mod_export(void) {
  svScope scope = svGetScopeFromName("Test.u");
  svSetScope(scope);
  svLogicVecVal vec;
  vec.aval = 0x42;
  vec.bval = 0x00;
  return read_logic8_mod_export(&vec);
}
