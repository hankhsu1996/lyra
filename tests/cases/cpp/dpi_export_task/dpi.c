#include <stdint.h>

/* The imported task drives the exported SV tasks (LRM 35.8): an exported task
   is called from an imported task in the DPI call chain. Each exported task's
   foreign entry returns the DPI disable-acknowledgment int, discarded here. A
   scalar output / inout crosses by pointer to its by-value carrier. */
extern int add_to_total(int delta);
extern int get_pair(int* lo, int* hi);
extern int accumulate(int* acc);

int drive(void) {
  add_to_total(5);
  add_to_total(37);

  int lo = 0;
  int hi = 0;
  get_pair(&lo, &hi); /* lo=3, hi=7 */

  int acc = 5;
  accumulate(&acc); /* acc=105 */

  add_to_total(lo + hi + acc); /* total += 115 */
  return 0;
}
