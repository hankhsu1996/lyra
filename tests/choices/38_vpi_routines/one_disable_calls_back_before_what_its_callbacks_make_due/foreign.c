/* Places disable callbacks on a block and a block nested in it, the outer
   one writing a variable a value-change callback watches, and keeps the order
   the three are called in. */
#include <stddef.h>
#include <sv_vpi_user.h>

typedef struct {
  int calls_so_far;
  int outer;
  int inner;
  int changed;
} Order;

static Order* Kept(void) {
  static Order order;
  return &order;
}

static vpiHandle Named(const char* name) {
  return vpi_handle_by_name((PLI_BYTE8*)name, NULL);
}

static PLI_INT32 OuterDisabled(p_cb_data data) {
  if (data->reason != cbDisable) return 0;
  Kept()->outer = ++Kept()->calls_so_far;
  s_vpi_value value = {.format = vpiIntVal, .value.integer = 1};
  vpi_put_value(Named("Top.poked"), &value, NULL, vpiNoDelay);
  return 0;
}

static PLI_INT32 InnerDisabled(p_cb_data data) {
  if (data->reason == cbDisable) Kept()->inner = ++Kept()->calls_so_far;
  return 0;
}

static PLI_INT32 Changed(p_cb_data data) {
  if (data->reason == cbValueChange) Kept()->changed = ++Kept()->calls_so_far;
  return 0;
}

int place(void) {
  s_cb_data outer = {
      .reason = cbDisable, .cb_rtn = OuterDisabled, .obj = Named("Top.outer")};
  s_cb_data inner = {
      .reason = cbDisable,
      .cb_rtn = InnerDisabled,
      .obj = Named("Top.outer.inner")};
  s_vpi_value value = {.format = vpiSuppressVal};
  s_cb_data changed = {
      .reason = cbValueChange,
      .cb_rtn = Changed,
      .obj = Named("Top.poked"),
      .value = &value};
  if (vpi_register_cb(&outer) == NULL || vpi_register_cb(&inner) == NULL ||
      vpi_register_cb(&changed) == NULL)
    return 1;
  return 0;
}

int outer_order(void) {
  return Kept()->outer;
}
int inner_order(void) {
  return Kept()->inner;
}
int changed_order(void) {
  return Kept()->changed;
}
