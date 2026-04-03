#include <stdint.h>

typedef uint8_t svLogic;

typedef struct {
  uint32_t aval;
  uint32_t bval;
} svLogicVecVal;

#define sv_0 0
#define sv_1 1
#define sv_z 2
#define sv_x 3

/* --- scalar logic --- */

extern int echo_logic_as_int(svLogic x);

int call_echo_logic_as_int(svLogic x) {
  return echo_logic_as_int(x);
}

extern svLogic get_logic_one(void);

int call_get_logic_one(void) {
  svLogic v = get_logic_one();
  return (v == sv_1) ? 1 : 0;
}

extern int check_all_four_logic(svLogic a, svLogic b, svLogic c, svLogic d);

int call_check_all_four_logic(void) {
  return check_all_four_logic(sv_0, sv_1, sv_x, sv_z);
}

/* --- logic vector input --- */

extern int read_logic8_aval_export(const svLogicVecVal* v);

int call_read_logic8_aval_export(void) {
  svLogicVecVal vec;
  vec.aval = 0xA5;
  vec.bval = 0x00;
  return read_logic8_aval_export(&vec);
}

/* --- logic vector output --- */

extern void write_logic8_export(svLogicVecVal* out);

int call_write_logic8_export(void) {
  svLogicVecVal vec;
  /* Pre-fill with garbage to verify wrapper does not read this */
  vec.aval = 0xDEADBEEF;
  vec.bval = 0xDEADBEEF;
  write_logic8_export(&vec);
  /* SV writes logic [7:0] = 8'hA5 (known, bval=0) */
  return (vec.aval == 0xA5 && vec.bval == 0x00) ? 1 : 0;
}

/* --- logic vector inout --- */

extern void flip_logic8_export(svLogicVecVal* inout);

int call_flip_logic8_export(void) {
  svLogicVecVal vec;
  vec.aval = 0x55;
  vec.bval = 0x00;
  flip_logic8_export(&vec);
  /* SV XORs aval with 0xFF -> 0xAA */
  return (vec.aval == 0xAA && vec.bval == 0x00) ? 1 : 0;
}

/* --- module-scoped packed param --- */

extern int read_logic8_mod_export(const svLogicVecVal* v);

int call_read_logic8_mod_export(void) {
  svLogicVecVal vec;
  vec.aval = 0x42;
  vec.bval = 0x00;
  return read_logic8_mod_export(&vec);
}

/* --- consecutive calls (aliasing/lifetime) --- */

extern int sum_logic8_avals(const svLogicVecVal* v);

int call_consecutive_export(void) {
  svLogicVecVal v1;
  svLogicVecVal v2;
  v1.aval = 0x10;
  v1.bval = 0x00;
  v2.aval = 0x20;
  v2.bval = 0x00;
  int r1 = sum_logic8_avals(&v1);
  int r2 = sum_logic8_avals(&v2);
  /* r1 should be 0x10, r2 should be 0x20 */
  return (r1 == 0x10 && r2 == 0x20) ? 1 : 0;
}
