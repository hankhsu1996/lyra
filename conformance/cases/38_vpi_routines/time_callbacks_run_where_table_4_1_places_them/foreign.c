/* Places the time callbacks of LRM 38.36.2 and keeps, for each, how often it
   was called, when, and what it read of the design then. */
#include <stddef.h>
#include <string.h>
#include <sv_vpi_user.h>

enum { kMaxPlaced = 16 };

/* One callback placed: its label, how often and when it was called, and
   the values of the variables it reads, read as it was called. */
typedef struct {
  const char* label;
  vpiHandle callback;
  int calls;
  PLI_UINT32 called_at;
  const char* reads[2];
  PLI_INT32 read[2];
} Placed;

typedef struct {
  Placed placed[kMaxPlaced];
  int count;
  int differences;
} State;

static State* Kept(void) {
  static State state;
  return &state;
}

static void Differ(const char* what, const char* of) {
  vpi_printf("difference: %s of '%s'\n", what, of);
  ++Kept()->differences;
}

static Placed* Find(const char* label) {
  State* state = Kept();
  for (int at = 0; at < state->count; ++at) {
    if (strcmp(state->placed[at].label, label) == 0) return &state->placed[at];
  }
  return NULL;
}

static vpiHandle Named(const char* name) {
  return vpi_handle_by_name((PLI_BYTE8*)name, NULL);
}

static PLI_INT32 IntOf(const char* name) {
  s_vpi_value value = {.format = vpiIntVal};
  vpi_get_value(Named(name), &value);
  return value.value.integer;
}

static void WriteInt(const char* name, PLI_INT32 number) {
  s_vpi_value value = {.format = vpiIntVal, .value.integer = number};
  vpi_put_value(Named(name), &value, NULL, vpiNoDelay);
}

static vpiHandle Register(
    PLI_INT32 reason, PLI_UINT32 time, PLI_INT32 (*routine)(p_cb_data),
    Placed* placed) {
  s_vpi_time when = {.type = vpiSimTime, .high = 0, .low = time};
  s_cb_data data = {
      .reason = reason,
      .cb_rtn = routine,
      .obj = NULL,
      .time = &when,
      .user_data = (PLI_BYTE8*)placed};
  return vpi_register_cb(&data);
}

static PLI_INT32 Called(p_cb_data data);

static Placed* Place(
    const char* label, PLI_INT32 reason, PLI_UINT32 time, const char* first,
    const char* second) {
  State* state = Kept();
  Placed* placed = &state->placed[state->count++];
  memset(placed, 0, sizeof *placed);
  placed->label = label;
  placed->reads[0] = first;
  placed->reads[1] = second;
  placed->callback = Register(reason, time, Called, placed);
  if (placed->callback == NULL) Differ("the callback was refused", label);
  return placed;
}

static PLI_INT32 Called(p_cb_data data) {
  Placed* placed = (Placed*)data->user_data;
  ++placed->calls;
  s_vpi_time now = {.type = vpiSimTime};
  vpi_get_time(NULL, &now);
  placed->called_at = now.low;
  if (data->time == NULL || data->time->type != vpiSimTime ||
      data->time->low != now.low) {
    Differ("the call was handed another time", placed->label);
  }
  for (int at = 0; at < 2; ++at) {
    if (placed->reads[at] != NULL) placed->read[at] = IntOf(placed->reads[at]);
  }
  if (strcmp(placed->label, "read write") == 0) WriteInt("Top.rw_written", 1);
  if (strcmp(placed->label, "end of time") == 0) WriteInt("Top.end_written", 1);
  if (strcmp(placed->label, "read only") == 0) {
    s_vpi_value value = {.format = vpiIntVal, .value.integer = 9};
    vpi_put_value(Named("Top.ro_target"), &value, NULL, vpiNoDelay);
    if (vpi_chk_error(NULL) == 0)
      Differ("a write at read-only time was not refused", "read only");
    if (Register(cbReadWriteSynch, 0, Called, placed) != NULL) {
      Differ(
          "a read-write callback at read-only time was not refused",
          "read only");
    }
    if (vpi_chk_error(NULL) == 0)
      Differ("the refusal reported no error", "read only");
  }
  if (strcmp(placed->label, "start of time") == 0 && placed->calls == 1) {
    /* LRM 38.36.2: delay zero inside a start-of-time callback is a callback
       in the same time slice. */
    Place("start of time again", cbAtStartOfSimTime, 0, NULL, NULL);
  }
  if (strcmp(placed->label, "nba synch") == 0) {
    /* Placed at 3 with a time of 4, it is due 4 later. */
    Place("end of time, placed later", cbAtEndOfSimTime, 4, NULL, NULL);
  }
  if (strcmp(placed->label, "after delay") == 0) {
    /* LRM 38.36.2: inside a callback of another reason, though it runs in the
       same region, delay zero is an error and no callback is made. */
    Placed inside = {.label = "inside after delay"};
    if (Register(cbAtStartOfSimTime, 0, Called, &inside) != NULL) {
      Differ(
          "a start-of-time callback for the slot under way was made",
          "after delay");
    }
    if (vpi_chk_error(NULL) == 0)
      Differ("the refusal reported no error", "after delay");
  }
  return 0;
}

int place_at_zero(void) {
  Kept()->differences = 0;
  Place("after delay", cbAfterDelay, 5, "Top.at_five", NULL);
  Place("after delay, nothing else then", cbAfterDelay, 20, NULL, NULL);
  Place("start of time", cbAtStartOfSimTime, 10, "Top.at_ten", NULL);
  Place("nba synch", cbNBASynch, 3, "Top.blocking_three", "Top.nba_three");
  Place("read write", cbReadWriteSynch, 4, NULL, NULL);
  Place("end of time", cbAtEndOfSimTime, 6, "Top.nba_six", NULL);
  Place("read only", cbReadOnlySynch, 7, "Top.nba_seven", "Top.follows_seven");
  Placed* removed = Place("removed", cbAfterDelay, 25, NULL, NULL);
  if (removed->callback != NULL && vpi_remove_cb(removed->callback) != 1) {
    Differ("removing a pending callback failed", "removed");
  }
  /* LRM 38.36.2: a start-of-time callback with a delay of zero once the slot
     is under way, outside a start-of-time callback, is an error and no
     callback is made. */
  Placed late = {.label = "late"};
  if (Register(cbAtStartOfSimTime, 0, Called, &late) != NULL) {
    Differ(
        "a start-of-time callback for a time already begun was made", "late");
  }
  if (vpi_chk_error(NULL) == 0) Differ("the refusal reported no error", "late");
  return Kept()->differences;
}

int place_next(void) {
  Place("next", cbNextSimTime, 0, NULL, NULL);
  return Kept()->differences;
}

int place_next_with_nothing_after(void) {
  Place("next, nothing after", cbNextSimTime, 0, NULL, NULL);
  return Kept()->differences;
}

int calls_of(const char* label) {
  Placed* placed = Find(label);
  return placed == NULL ? -1 : placed->calls;
}

int called_at(const char* label) {
  Placed* placed = Find(label);
  return placed == NULL ? -1 : (int)placed->called_at;
}

int read_of(const char* label, int which) {
  Placed* placed = Find(label);
  return placed == NULL ? -1 : placed->read[which];
}

int differences_so_far(void) {
  return Kept()->differences;
}
