/* Places disable callbacks on named blocks, tasks and a function, keeping
   how often each was called and when. One removes itself and the callback
   placed after it as it is called. */
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <sv_vpi_user.h>

enum { kMaxWatches = 11 };

typedef struct {
  const char* name;
  vpiHandle callback;
  int calls;
  PLI_UINT32 at;
} Watch;

typedef struct {
  Watch watches[kMaxWatches];
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

static Watch* Find(const char* name) {
  State* state = Kept();
  for (int at = 0; at < state->count; ++at) {
    if (strcmp(state->watches[at].name, name) == 0) return &state->watches[at];
  }
  return NULL;
}

static PLI_INT32 Disabled(p_cb_data data) {
  Watch* watch = (Watch*)data->user_data;
  if (data->reason != cbDisable)
    Differ("a call of another reason", watch->name);
  ++watch->calls;
  s_vpi_time now = {.type = vpiSimTime};
  vpi_get_time(NULL, &now);
  watch->at = now.low;
  return 0;
}

static PLI_INT32 DisabledThenRemoves(p_cb_data data) {
  Disabled(data);
  Watch* self = (Watch*)data->user_data;
  Watch* next = Find("Top.removed, removed");
  if (vpi_remove_cb(next->callback) != 1)
    Differ("removing the next callback failed", self->name);
  if (vpi_remove_cb(self->callback) != 1)
    Differ("removing itself failed", self->name);
  return 0;
}

static void Place(
    const char* label, const char* name, PLI_INT32 (*routine)(p_cb_data)) {
  State* state = Kept();
  if (state->count == kMaxWatches) abort();
  Watch* watch = &state->watches[state->count++];
  memset(watch, 0, sizeof *watch);
  watch->name = label;
  vpiHandle handle = vpi_handle_by_name((PLI_BYTE8*)name, NULL);
  if (handle == NULL) {
    Differ("nothing is reached", name);
    return;
  }
  s_cb_data data = {
      .reason = cbDisable,
      .cb_rtn = routine,
      .obj = handle,
      .user_data = (PLI_BYTE8*)watch};
  watch->callback = vpi_register_cb(&data);
  if (watch->callback == NULL) Differ("the callback was refused", name);
}

int watch_all(void) {
  Place("Top.work", "Top.work", Disabled);
  Place("Top.wait_long", "Top.wait_long", Disabled);
  Place("Top.twice", "Top.twice", Disabled);
  Place("Top.outer", "Top.outer", Disabled);
  Place("Top.outer.inner", "Top.outer.inner", Disabled);
  Place("Top.chain_head", "Top.chain_head", Disabled);
  Place("Top.chain_tail", "Top.chain_tail", Disabled);
  Place("Top.done_early", "Top.done_early", Disabled);
  Place("Top.twice_disabled", "Top.twice_disabled", Disabled);
  Place("Top.removed, removing", "Top.removed", DisabledThenRemoves);
  Place("Top.removed, removed", "Top.removed", Disabled);
  return Kept()->differences;
}

int calls_of(const char* name) {
  Watch* watch = Find(name);
  return watch == NULL ? -1 : watch->calls;
}

int called_at(const char* name) {
  Watch* watch = Find(name);
  return watch == NULL ? -1 : (int)watch->at;
}

int differences_so_far(void) {
  return Kept()->differences;
}
