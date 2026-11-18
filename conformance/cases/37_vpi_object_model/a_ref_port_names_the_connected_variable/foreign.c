/* Reaches the reference and a part of it, and what each refers to; every
   answer that is not the one stated here is counted. */
#include <stddef.h>
#include <string.h>
#include <sv_vpi_user.h>

/* How many answers differed from the ones stated here. */
static int* Differences(void) {
  static int count;
  return &count;
}

static void Differ(const char* what) {
  vpi_printf("difference: %s\n", what);
  ++*Differences();
}

static vpiHandle Named(const char* name) {
  vpiHandle h = vpi_handle_by_name((PLI_BYTE8*)name, NULL);
  if (h == NULL) Differ("a name reaches nothing");
  return h;
}

static void ExpectSame(vpiHandle a, vpiHandle b, const char* what) {
  if (a == NULL || b == NULL || vpi_compare_objects(a, b) != 1) Differ(what);
}

int look(void) {
  *Differences() = 0;
  vpiHandle r = Named("Top.u1.r");
  if (r != NULL && vpi_get(vpiType, r) != vpiRefObj)
    Differ("the port's name is no reference object");
  ExpectSame(
      r == NULL ? NULL : vpi_handle(vpiActual, r), Named("Top.a"),
      "the reference's actual object is not the variable");
  vpiHandle bit = r == NULL ? NULL : vpi_handle_by_index(r, 0);
  if (bit != NULL && vpi_get(vpiType, bit) != vpiRefObj)
    Differ("a part of the reference is no reference object");
  ExpectSame(
      bit == NULL ? NULL : vpi_handle(vpiParent, bit), r,
      "a part's parent is not the reference");
  ExpectSame(
      bit == NULL ? NULL : vpi_handle(vpiActual, bit), Named("Top.a[0]"),
      "a part's actual object is not that part");
  vpiHandle deep = Named("Top.u1.below.deep");
  ExpectSame(
      deep == NULL ? NULL : vpi_handle(vpiActual, deep), Named("Top.a"),
      "a reference bound to a reference names other than the variable");
  s_vpi_value one = {.format = vpiScalarVal, .value.scalar = vpi1};
  if (bit != NULL) vpi_put_value(bit, &one, NULL, vpiNoDelay);
  if (vpi_chk_error(NULL) != 0) Differ("a write through the reference failed");
  return *Differences();
}
