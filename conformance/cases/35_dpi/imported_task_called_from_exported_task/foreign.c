/* Two imported tasks, one reached from inside the exported task the other
   called, and an exported function reached from that inner one. Neither frame
   here holds anything across the suspensions the exported task executes around
   the inner call, which is what LRM 35.5.1.5 asks of C code that can be active
   in more than one execution thread.

   Then a chain of tasks that each call back an exported task, which in turn
   calls the next one in, so the foreign calls nest; each passes the exported
   task's answer on, which is how a disable reaches the outermost (LRM 35.9). */
#include <svdpi.h>

/* The exported subroutines called from here, declared as the design
   exports them (LRM 35.5.6, Annex H). */
int32_t middle(int32_t);
void note(int32_t);
int32_t relay(int32_t);
int32_t leaf(int32_t);
int32_t bottom(void);

int32_t outer_call(int32_t amount) {
  middle(amount);
  return 0;
}

int32_t inner_call(int32_t value) {
  note(value + 1);
  return 0;
}

int32_t outer_c(int32_t which) {
  return relay(which);
}

int32_t inner_c(int32_t which) {
  return leaf(which);
}

int32_t deepest_c(void) {
  return bottom();
}
