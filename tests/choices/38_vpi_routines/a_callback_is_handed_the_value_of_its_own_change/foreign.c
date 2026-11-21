/* Places two value-change callbacks on one variable; the first writes it again
   the first time it is called. Each keeps the values it was handed, in order.
 */
#include <stddef.h>
#include <sv_vpi_user.h>

enum { kKept = 4 };

typedef struct {
  PLI_INT32 seen[kKept];
  int calls;
  int writes_again;
} Watch;

static Watch* Watches(void) {
  static Watch watches[2];
  return watches;
}

static PLI_INT32 Changed(p_cb_data data) {
  Watch* watch = (Watch*)data->user_data;
  if (data->value != NULL && watch->calls < kKept) {
    watch->seen[watch->calls] = data->value->value.integer;
  }
  ++watch->calls;
  if (watch->writes_again) {
    watch->writes_again = 0;
    s_vpi_value value = {.format = vpiIntVal, .value.integer = 2};
    vpi_put_value(data->obj, &value, NULL, vpiNoDelay);
  }
  return 0;
}

int place(void) {
  vpiHandle x = vpi_handle_by_name((PLI_BYTE8*)"Top.x", NULL);
  if (x == NULL) return 1;
  Watches()[0].writes_again = 1;
  for (int at = 0; at < 2; ++at) {
    s_vpi_value value = {.format = vpiIntVal};
    s_cb_data data = {
        .reason = cbValueChange,
        .cb_rtn = Changed,
        .obj = x,
        .value = &value,
        .user_data = (PLI_BYTE8*)&Watches()[at]};
    if (vpi_register_cb(&data) == NULL) return 1;
  }
  return 0;
}

int calls_of(int which) {
  return Watches()[which].calls;
}
int seen_by(int which, int call) {
  return Watches()[which].seen[call];
}
