// The emitted program provides `sv_double` as an `extern "C"` symbol (the DPI-C
// export wrapper). This imported function calls it back.
extern int sv_double(int x);

int round_trip(int x) {
  return sv_double(x);
}
