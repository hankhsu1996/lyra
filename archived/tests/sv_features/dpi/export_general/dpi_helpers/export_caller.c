/* C helper that calls Lyra-exported DPI functions. */

/* Exported by Lyra via export "DPI-C" */
extern int add_two(int a, int b);
extern int get_forty_two(void);

/* Called from SV via import "DPI-C" to exercise the export wrappers. */
int call_get_forty_two(void) {
  return get_forty_two();
}

int call_add_two(int a, int b) {
  return add_two(a, b);
}
