/* An imported task that calls in again after the run has ended: the exported
   function twice, then the exported task, and notes whether the task came
   back. */
#include <svdpi.h>

/* Defined by the design (LRM 35.5.6). */
int32_t answer(void);
int step(void);

/* Kept here rather than handed to the design, which runs nothing once the
   run has ended. */
static int* TaskReturned(void) {
  static int returned = 0;
  return &returned;
}

int drive(void) {
  answer();
  answer();
  step();
  *TaskReturned() = 1;
  return 0;
}

int the_task_returned(void) {
  return *TaskReturned();
}
