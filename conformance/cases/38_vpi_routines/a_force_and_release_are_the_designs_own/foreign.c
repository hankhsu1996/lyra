/* Forces and releases a variable, and asks to force a part of one; every
   routine answering other than as stated here is counted. */
#include <stddef.h>
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
  if (h == NULL) Differ("a variable is reached by no name");
  return h;
}

int force_it(void) {
  *Differences() = 0;
  vpiHandle f = Named("Top.f");
  s_vpi_value value = {.format = vpiHexStrVal, .value.str = (PLI_BYTE8*)"55"};
  if (f != NULL) vpi_put_value(f, &value, NULL, vpiForceFlag);
  if (vpi_chk_error(NULL) != 0) Differ("the force was refused");
  return *Differences();
}

int release_it(int* after) {
  *Differences() = 0;
  vpiHandle f = Named("Top.f");
  s_vpi_value value = {.format = vpiIntVal};
  if (f != NULL) vpi_put_value(f, &value, NULL, vpiReleaseFlag);
  if (vpi_chk_error(NULL) != 0) Differ("the release was refused");
  *after = value.value.integer;
  return *Differences();
}

/* Asks for a force of each part a procedural force does not take; that
   nothing of the variable changes is what the design checks, since the
   standard leaves open whether the call reports an error. */
int force_a_part(void) {
  *Differences() = 0;
  const char* parts[] = {
      "Top.ps.hi", "Top.ps[0]", "Top.f[3]", "Top.arr[2]", "Top.fixed"};
  for (int at = 0; at < 5; ++at) {
    vpiHandle part = Named(parts[at]);
    s_vpi_value value = {.format = vpiIntVal, .value.integer = 0};
    if (part == NULL) continue;
    vpi_put_value(part, &value, NULL, vpiForceFlag);
  }
  return *Differences();
}
