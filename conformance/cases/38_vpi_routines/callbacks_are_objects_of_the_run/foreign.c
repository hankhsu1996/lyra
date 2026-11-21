/* Places callbacks and reads them back as objects: their type and what they
   were registered with, the callbacks an object and a time queue relate to,
   the time queues, and what removing and releasing a callback does. */
#include <stddef.h>
#include <sv_vpi_user.h>

typedef struct {
  int value_changes;
  int delay_calls;
  int removed_calls;
  int differences;
} State;

static State* Kept(void) {
  static State state;
  return &state;
}

static int* Tag(void) {
  static int tag;
  return &tag;
}

static void Differ(const char* what) {
  vpi_printf("difference: %s\n", what);
  ++Kept()->differences;
}

static PLI_INT32 OnValue(p_cb_data data) {
  if (data->reason == cbValueChange) ++Kept()->value_changes;
  return 0;
}

static PLI_INT32 OnDelay(p_cb_data data) {
  if (data->reason == cbAfterDelay) ++Kept()->delay_calls;
  return 0;
}

static PLI_INT32 OnRemoved(p_cb_data data) {
  if (data->reason == cbAfterDelay) ++Kept()->removed_calls;
  return 0;
}

static vpiHandle* SelfRemoving(void) {
  static vpiHandle handle;
  return &handle;
}

/* Removes itself, then reads the object it was handed, which stays its own
   until the call returns (LRM 38.36). */
static PLI_INT32 OnValueRemovingItself(p_cb_data data) {
  vpiHandle x = vpi_handle_by_name((PLI_BYTE8*)"Top.x", NULL);
  if (vpi_remove_cb(*SelfRemoving()) != 1)
    Differ("a callback could not remove itself");
  if (data->obj == NULL || !vpi_compare_objects(data->obj, x))
    Differ("a callback that removed itself lost the object it was handed");
  return 0;
}

static PLI_INT32 OnEnd(p_cb_data data) {
  if (data->reason != cbEndOfSimulation)
    Differ("an end call of another reason");
  return 0;
}

static vpiHandle After(PLI_UINT32 delay, PLI_INT32 (*routine)(p_cb_data)) {
  s_vpi_time when = {.type = vpiSimTime, .high = 0, .low = delay};
  s_cb_data data = {.reason = cbAfterDelay, .cb_rtn = routine, .time = &when};
  vpiHandle callback = vpi_register_cb(&data);
  if (callback == NULL) Differ("an after-delay callback was refused");
  return callback;
}

/* Whether stepping `iterator` meets an object `wanted` is. */
static int Meets(vpiHandle iterator, vpiHandle wanted) {
  int met = 0;
  if (iterator == NULL) return 0;
  for (vpiHandle each = vpi_scan(iterator); each != NULL;
       each = vpi_scan(iterator)) {
    if (vpi_compare_objects(each, wanted)) met = 1;
  }
  return met;
}

int inspect(void) {
  vpiHandle x = vpi_handle_by_name((PLI_BYTE8*)"Top.x", NULL);
  s_vpi_time time = {.type = vpiSimTime};
  s_vpi_value value = {.format = vpiIntVal};
  s_cb_data data = {
      .reason = cbValueChange,
      .cb_rtn = OnValue,
      .obj = x,
      .time = &time,
      .value = &value,
      .user_data = (PLI_BYTE8*)Tag()};
  vpiHandle on_x = vpi_register_cb(&data);
  if (on_x == NULL) {
    Differ("a value-change callback was refused");
    return Kept()->differences;
  }
  s_cb_data removing = {
      .reason = cbValueChange, .cb_rtn = OnValueRemovingItself, .obj = x};
  *SelfRemoving() = vpi_register_cb(&removing);
  if (*SelfRemoving() == NULL)
    Differ("a value-change callback that removes itself was refused");
  if (vpi_get(vpiType, on_x) != vpiCallback)
    Differ("a callback is not of type vpiCallback");

  /* LRM 38.8: the registration, read back. */
  s_cb_data info = {0};
  vpi_get_cb_info(on_x, &info);
  if (info.reason != cbValueChange) Differ("the reason read back differs");
  if (info.cb_rtn != OnValue) Differ("the routine read back differs");
  if (info.user_data != (PLI_BYTE8*)Tag())
    Differ("the user data read back differs");
  if (info.obj == NULL || !vpi_compare_objects(info.obj, x))
    Differ("the object read back differs");
  if (info.time == NULL || info.time->type != vpiSimTime)
    Differ("the time type read back differs");
  if (info.value == NULL || info.value->format != vpiIntVal)
    Differ("the value format read back differs");

  vpiHandle at_five = After(5, OnDelay);
  vpiHandle at_seven = After(7, OnRemoved);
  s_cb_data end = {.reason = cbEndOfSimulation, .cb_rtn = OnEnd};
  vpiHandle on_end = vpi_register_cb(&end);
  if (on_end == NULL) Differ("an end-of-simulation callback was refused");

  /* LRM 37.80: the callbacks an object relates to, and with no object those
     related to none of the objects of the diagram. */
  if (!Meets(vpi_iterate(vpiCallback, x), on_x))
    Differ("x does not relate to its callback");
  if (!Meets(vpi_iterate(vpiCallback, NULL), on_end))
    Differ("no object does not relate to the end callback");
  if (Meets(vpi_iterate(vpiCallback, NULL), on_x))
    Differ("no object relates to x's callback");
  /* A task is none of the diagram's objects, so a callback placed on one is
     related to none of them, and a task is related to no callbacks. */
  vpiHandle task = vpi_handle_by_name((PLI_BYTE8*)"Top.work", NULL);
  s_cb_data disable = {.reason = cbDisable, .cb_rtn = OnEnd, .obj = task};
  vpiHandle on_task = task == NULL ? NULL : vpi_register_cb(&disable);
  if (on_task == NULL) Differ("a disable callback on a task was refused");
  if (!Meets(vpi_iterate(vpiCallback, NULL), on_task))
    Differ("no object does not relate to a task's callback");
  if (task != NULL && vpi_iterate(vpiCallback, task) != NULL)
    Differ("a task relates to callbacks");

  /* LRM 37.81: the time queues, in increasing time, and the callbacks each
     holds. */
  vpiHandle queues = vpi_iterate(vpiTimeQueue, NULL);
  PLI_UINT32 last = 0;
  int saw_five = 0;
  int saw_seven = 0;
  if (queues == NULL) Differ("no time queue was found");
  for (vpiHandle queue = queues == NULL ? NULL : vpi_scan(queues);
       queue != NULL; queue = vpi_scan(queues)) {
    if (vpi_get(vpiType, queue) != vpiTimeQueue)
      Differ("a time queue is not of type vpiTimeQueue");
    s_vpi_time at = {.type = vpiSimTime};
    vpi_get_time(queue, &at);
    if (at.low < last) Differ("the time queues are not in increasing time");
    last = at.low;
    if (at.low == 5) {
      saw_five = 1;
      if (!Meets(vpi_iterate(vpiCallback, queue), at_five))
        Differ("the queue at 5 does not hold its callback");
    }
    if (at.low == 7) saw_seven = 1;
  }
  if (!saw_five || !saw_seven)
    Differ("a time queue a callback is at was not found");

  /* LRM 38.39: a removed callback's handle is no longer valid. */
  if (vpi_remove_cb(at_seven) != 1) Differ("removing a callback failed");
  if (vpi_remove_cb(at_seven) != 0)
    Differ("a removed callback was removed again");
  if (vpi_chk_error(NULL) == 0)
    Differ("removing a removed callback reported no error");

  /* LRM 38.38: releasing a handle leaves what it refers to. */
  if (vpi_release_handle(at_five) != 1)
    Differ("releasing a callback's handle failed");
  return Kept()->differences;
}

/* A time callback removed before its time keeps no time queue (LRM 38.39). */
int remove_late(void) {
  vpiHandle late = After(100, OnRemoved);
  if (late == NULL || vpi_remove_cb(late) != 1) {
    Differ("removing a callback just placed failed");
    return Kept()->differences;
  }
  vpiHandle queues = vpi_iterate(vpiTimeQueue, NULL);
  for (vpiHandle queue = queues == NULL ? NULL : vpi_scan(queues);
       queue != NULL; queue = vpi_scan(queues)) {
    s_vpi_time at = {.type = vpiSimTime};
    vpi_get_time(queue, &at);
    if (at.low >= 100) Differ("a removed callback kept its time queue");
  }
  return Kept()->differences;
}

int value_changes(void) {
  return Kept()->value_changes;
}
int delay_calls(void) {
  return Kept()->delay_calls;
}
int removed_calls(void) {
  return Kept()->removed_calls;
}
int differences_so_far(void) {
  return Kept()->differences;
}
