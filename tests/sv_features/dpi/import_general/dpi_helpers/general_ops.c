#include <string.h>

// -- shared utility --
int string_length(const char* s) { return (int)strlen(s); }

// -- output int --
void set_int(int* out) { *out = 42; }

// -- inout int --
void double_int(int* val) { *val = (*val) * 2; }

// -- output string --
void get_greeting(const char** out) { *out = "hello from C"; }

// -- inout string --
void append_suffix(const char** val) {
  if (*val != 0 && strcmp(*val, "base") == 0) {
    *val = "base_modified";
  } else {
    *val = "unknown_input";
  }
}

// -- output null string --
void get_null_string(const char** out) { *out = 0; }

// -- inout string overwrite with null --
void nullify_string(const char** val) { *val = 0; }

// -- chandle --
static int sentinel = 99;
void get_handle(void** out) { *out = &sentinel; }
void* get_handle_ret(void) { return &sentinel; }
int check_handle(void* h) { return (h == &sentinel) ? 1 : 0; }
void roundtrip_handle(void** h) {
  // Just verify we can read and write through the pointer.
  // If input was our sentinel, keep it. Otherwise set it.
  if (*h != &sentinel) {
    *h = &sentinel;
  }
}

// -- packed bit[7:0] (unsigned char at C boundary) --
void set_byte(unsigned char* out) { *out = 0xAB; }
void double_byte(unsigned char* val) {
  unsigned char v = *val;
  *val = v * 2;
}

// -- packed bit[31:0] (uint32_t at C boundary) --
void set_word(unsigned int* out) { *out = 12345; }
void double_word(unsigned int* val) { *val = (*val) * 2; }

// -- packed bit[63:0] (uint64_t at C boundary, mapped to long long) --
void set_dword(unsigned long long* out) { *out = 123456789ULL; }
void double_dword(unsigned long long* val) { *val = (*val) * 2; }

// -- packed struct (32-bit total: two 16-bit fields) --
void set_packed32(unsigned int* out) { *out = 0x00030007; }
void inc_packed32(unsigned int* val) { *val = (*val) + 1; }

// -- packed union (32-bit) --
void set_punion32(unsigned int* out) { *out = 0xDEAD; }
void inc_punion32(unsigned int* val) { *val = (*val) + 1; }

// -- 1-bit (bit scalar) --
void set_bit(unsigned char* out) { *out = 1; }
void flip_bit(unsigned char* val) { *val = (*val) ^ 1; }

// -- 64-bit high-bit preservation --
void set_high_bit64(unsigned long long* out) { *out = 0xDEADBEEF12345678ULL; }
void mirror_dword(unsigned long long* val) { (void)*val; }
