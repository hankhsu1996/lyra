/* Places size-change callbacks on a queue, a dynamic array, an associative
   array, a string, a randomized dynamic array, a queue in an entry of an
   associative array, a string member of a structure and a queue element of
   an array, and a value-change callback on the queue's first
   element, keeping how often each was called, what it was handed, and the
   order the queue's two were called in. */
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sv_vpi_user.h>

enum { kMaxWatches = 10, kLogLength = 32 };

typedef struct {
  const char* name;
  int calls;
  PLI_INT32 last;
  char letter;
} Watch;

typedef struct {
  Watch watches[kMaxWatches];
  int count;
  int differences;
  char log[kLogLength];
  int logged;
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

static PLI_INT32 Called(p_cb_data data) {
  Watch* watch = (Watch*)data->user_data;
  State* state = Kept();
  ++watch->calls;
  if (data->value == NULL || data->value->format != vpiIntVal) {
    Differ("a call was handed no integer", watch->name);
  } else {
    watch->last = data->value->value.integer;
  }
  if (watch->letter != 0 && state->logged < kLogLength - 1) {
    state->log[state->logged++] = watch->letter;
  }
  return 0;
}

static void Place(
    const char* name, const char* object, PLI_INT32 reason, char letter) {
  State* state = Kept();
  if (state->count == kMaxWatches) abort();
  Watch* watch = &state->watches[state->count++];
  memset(watch, 0, sizeof *watch);
  watch->name = name;
  watch->last = -1;
  watch->letter = letter;
  vpiHandle handle = vpi_handle_by_name((PLI_BYTE8*)object, NULL);
  if (handle == NULL) {
    Differ("nothing is reached", object);
    return;
  }
  s_vpi_value value = {.format = vpiIntVal};
  s_cb_data data = {
      .reason = reason,
      .cb_rtn = Called,
      .obj = handle,
      .value = &value,
      .user_data = (PLI_BYTE8*)watch};
  if (vpi_register_cb(&data) == NULL) Differ("the callback was refused", name);
}

int watch_all(void) {
  Place("q", "Top.q", cbSizeChange, 'S');
  Place("q[0]", "Top.q[0]", cbValueChange, 'V');
  Place("da", "Top.da", cbSizeChange, 0);
  Place("aa", "Top.aa", cbSizeChange, 0);
  Place("s", "Top.s", cbSizeChange, 0);
  Place("rd", "Top.rd", cbSizeChange, 0);
  Place("qa[1]", "Top.qa[1]", cbSizeChange, 0);
  Place("st.s", "Top.st.s", cbSizeChange, 0);
  Place("aq[1]", "Top.aq[1]", cbSizeChange, 0);
  return Kept()->differences;
}

int calls_of(const char* name) {
  Watch* watch = Find(name);
  return watch == NULL ? -1 : watch->calls;
}

int last_of(const char* name) {
  Watch* watch = Find(name);
  return watch == NULL ? -1 : watch->last;
}

const char* order(void) {
  return Kept()->log;
}

int differences_so_far(void) {
  return Kept()->differences;
}
