/* Places value-change callbacks on the variables and parts the design names,
   and keeps what each call was handed, which the design asks for by name. */
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sv_vpi_user.h>

enum { kMaxWatches = 20 };

/* One callback placed, and what its calls were handed. */
typedef struct {
  const char* name;
  vpiHandle callback;
  int calls;
  int value_was_null;
  PLI_INT32 last_int;
  double last_real;
  PLI_INT32 last_index;
  PLI_UINT32 last_time;
  int removes_itself;
  int toggles_notifier;
  const char* removes;
} Watch;

/* Every callback placed, how many answers differed from the ones stated here,
   and the bit a callback on `i` toggles. */
typedef struct {
  Watch watches[kMaxWatches];
  int count;
  int differences;
  vpiHandle notifier;
} State;

static State* Kept(void) {
  static State state;
  return &state;
}

static void Differ(const char* what, const char* of) {
  vpi_printf("difference: %s of '%s'\n", what, of);
  ++Kept()->differences;
}

static Watch* Find(const char* name) {
  State* state = Kept();
  for (int at = 0; at < state->count; ++at) {
    if (strcmp(state->watches[at].name, name) == 0) return &state->watches[at];
  }
  return NULL;
}

static void Toggle(vpiHandle bit) {
  s_vpi_value value = {.format = vpiScalarVal};
  vpi_get_value(bit, &value);
  value.value.scalar = value.value.scalar == vpi1 ? vpi0 : vpi1;
  vpi_put_value(bit, &value, NULL, vpiNoDelay);
}

static PLI_INT32 Changed(p_cb_data data) {
  Watch* watch = (Watch*)data->user_data;
  ++watch->calls;
  if (data->reason != cbValueChange)
    Differ("a call of another reason", watch->name);
  watch->value_was_null = data->value == NULL;
  if (data->value != NULL) {
    switch (data->value->format) {
      case vpiIntVal:
        watch->last_int = data->value->value.integer;
        break;
      case vpiScalarVal:
        watch->last_int = data->value->value.scalar;
        break;
      case vpiRealVal:
        watch->last_real = data->value->value.real;
        break;
      default:
        Differ("a value in another format", watch->name);
    }
  }
  watch->last_index = data->index;
  if (data->time != NULL && data->time->type == vpiSimTime) {
    watch->last_time = data->time->low;
  }
  if (watch->removes_itself && watch->callback != NULL) {
    if (vpi_remove_cb(watch->callback) != 1)
      Differ("removing itself failed", watch->name);
    watch->callback = NULL;
  }
  if (watch->removes != NULL) {
    Watch* other = Find(watch->removes);
    if (other != NULL && other->callback != NULL) {
      if (vpi_remove_cb(other->callback) != 1)
        Differ("removing another failed", watch->name);
      other->callback = NULL;
    }
  }
  if (watch->toggles_notifier) Toggle(Kept()->notifier);
  return 0;
}

static Watch* Place(const char* name, vpiHandle object, PLI_INT32 format) {
  State* state = Kept();
  if (state->count == kMaxWatches) abort();
  Watch* watch = &state->watches[state->count++];
  memset(watch, 0, sizeof *watch);
  watch->name = name;
  watch->last_int = -1;
  watch->last_real = -1.0;
  watch->last_index = -1;
  if (object == NULL) {
    Differ("nothing is reached", name);
    return watch;
  }
  s_vpi_time time = {.type = vpiSimTime};
  s_vpi_value value = {.format = format};
  s_cb_data data = {
      .reason = cbValueChange,
      .cb_rtn = Changed,
      .obj = object,
      .time = &time,
      .value = &value,
      .user_data = (PLI_BYTE8*)watch};
  watch->callback = vpi_register_cb(&data);
  if (watch->callback == NULL) Differ("the callback was refused", name);
  return watch;
}

static vpiHandle Named(const char* name) {
  return vpi_handle_by_name((PLI_BYTE8*)name, NULL);
}

int watch_all(void) {
  Kept()->differences = 0;
  Kept()->notifier = Named("Top.n");
  Place("Top.i", Named("Top.i"), vpiIntVal)->toggles_notifier = 1;
  vpiHandle lv = Named("Top.lv");
  Place(
      "Top.lv[3]", lv == NULL ? NULL : vpi_handle_by_index(lv, 3),
      vpiScalarVal);
  Place("Top.ps.hi", Named("Top.ps.hi"), vpiIntVal);
  Place("Top.ua[2]", Named("Top.ua[2]"), vpiIntVal);
  Place("Top.r", Named("Top.r"), vpiRealVal);
  Place("Top.h", Named("Top.h"), vpiObjTypeVal);
  Place("Top.j", Named("Top.j"), vpiIntVal)->removes_itself = 1;
  Place("Top.k first", Named("Top.k"), vpiIntVal)->removes = "Top.k second";
  Place("Top.k second", Named("Top.k"), vpiIntVal);
  Place("Top.us.b", Named("Top.us.b"), vpiIntVal);
  Place("Top.q[0]", Named("Top.q[0]"), vpiIntVal);
  Place("Top.tu.b", Named("Top.tu.b"), vpiIntVal);
  Place("Top.va[5]", Named("Top.va[5]"), vpiIntVal);
  Place("Top.d[1]", Named("Top.d[1]"), vpiIntVal);
  Place("Top.holders[1].held", Named("Top.holders[1].held"), vpiObjTypeVal);
  Place("Top.handles[1]", Named("Top.handles[1]"), vpiObjTypeVal);
  Place("Top.mu.obj", Named("Top.mu.obj"), vpiObjTypeVal);
  Place("Top.keeper.kept", Named("Top.keeper.kept"), vpiObjTypeVal);
  return Kept()->differences;
}

void poke_i(int number) {
  s_vpi_value value = {.format = vpiIntVal, .value.integer = number};
  vpi_put_value(Named("Top.i"), &value, NULL, vpiNoDelay);
}

int calls_of(const char* name) {
  Watch* watch = Find(name);
  return watch == NULL ? -1 : watch->calls;
}

int last_int_of(const char* name) {
  Watch* watch = Find(name);
  return watch == NULL ? -1 : watch->last_int;
}

double last_real_of(const char* name) {
  Watch* watch = Find(name);
  return watch == NULL ? -1.0 : watch->last_real;
}

int last_index_of(const char* name) {
  Watch* watch = Find(name);
  return watch == NULL ? -1 : watch->last_index;
}

int last_time_of(const char* name) {
  Watch* watch = Find(name);
  return watch == NULL ? -1 : (int)watch->last_time;
}

int value_was_null(const char* name) {
  Watch* watch = Find(name);
  return watch == NULL ? -1 : watch->value_was_null;
}

int differences_so_far(void) {
  return Kept()->differences;
}
