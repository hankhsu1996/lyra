/* The emitted program provides `triple` as an `extern "C"` symbol (the DPI-C
   export wrapper for the $unit-scope function). This imported function calls it
   back. */
extern int triple(int x);

int call_unit(int x) {
  return triple(x);
}
