/* The emitted program provides `triple` as an `extern "C"` symbol (the DPI-C
   export wrapper for the package function). This imported function calls it back. */
extern int triple(int x);

int call_pkg(int x) {
  return triple(x);
}
