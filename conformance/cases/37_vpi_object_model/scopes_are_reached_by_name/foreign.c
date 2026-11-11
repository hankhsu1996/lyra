/* Keeps each name `%m` printed, then reaches the scope of each by that name and
   compares the full name it answers; and reaches scopes by the other forms of a
   name LRM 38.21 admits. Every answer that is not the one stated here is
   counted. */
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sv_vpi_user.h>

enum { kKept = 16 };

struct Kept {
  int count;
  char names[kKept][64];
};

static struct Kept* KeptNames(void) {
  static struct Kept kept;
  return &kept;
}

/* `name` is what `%m` must have printed, or empty where the design does not
   state it (an instance's own `%m`, whose name is reached as printed). */
void keep(const char* written, const char* name) {
  struct Kept* kept = KeptNames();
  if (name[0] != '\0' && strcmp(written, name) != 0) {
    vpi_printf("difference: %%m printed '%s', expected '%s'\n", written, name);
    strcpy(kept->names[kept->count], "(differs)");
  } else {
    snprintf(kept->names[kept->count], sizeof kept->names[0], "%s", written);
  }
  ++kept->count;
}

static int Differ(const char* what) {
  vpi_printf("difference: %s\n", what);
  return 1;
}

static int ExpectFull(vpiHandle h, const char* full) {
  if (h == NULL) {
    vpi_printf("difference: '%s' reaches nothing\n", full);
    return 1;
  }
  const char* got = vpi_get_str(vpiFullName, h);
  if (got == NULL || strcmp(got, full) != 0) {
    vpi_printf(
        "difference: the full name is '%s', expected '%s'\n",
        got == NULL ? "(null)" : got, full);
    return 1;
  }
  return 0;
}

int look(void) {
  int differences = 0;
  const struct Kept* kept = KeptNames();
  if (kept->count != 9) differences += Differ("not every scope printed its %m");
  for (int i = 0; i < kept->count; ++i) {
    differences += ExpectFull(
        vpi_handle_by_name((PLI_BYTE8*)kept->names[i], NULL), kept->names[i]);
  }

  vpiHandle top = vpi_handle_by_name((PLI_BYTE8*)"Top", NULL);
  differences += ExpectFull(top, "Top");
  vpiHandle u = vpi_handle_by_name((PLI_BYTE8*)"Top.u", NULL);
  differences += ExpectFull(u, "Top.u");
  if (u != NULL && vpi_get(vpiType, u) != vpiModuleArray)
    differences += Differ("Top.u is no module array");
  differences +=
      ExpectFull(vpi_handle_by_name((PLI_BYTE8*)"Top.g", NULL), "Top.g");
  /* The loop `g` is the scope's first generate construct and the unnamed
     `if` its second (LRM 27.6). */
  differences += ExpectFull(
      vpi_handle_by_name((PLI_BYTE8*)"Top.genblk2", NULL), "Top.genblk2");
  differences +=
      ExpectFull(vpi_handle_by_name((PLI_BYTE8*)"Top.main", NULL), "Top.main");
  differences +=
      ExpectFull(vpi_handle_by_name((PLI_BYTE8*)"Top.t", NULL), "Top.t");
  if (top != NULL) {
    differences +=
        ExpectFull(vpi_handle_by_name((PLI_BYTE8*)"g[1].l", top), "Top.g[1].l");
    differences += ExpectFull(
        vpi_handle_by_name((PLI_BYTE8*)"main.inner", top), "Top.main.inner");
    /* Within a scope only (LRM 38.21): a package's name from there names
       nothing. */
    if (vpi_handle_by_name((PLI_BYTE8*)"P::pt", top) != NULL)
      differences += Differ("P::pt is reached from within Top");
    /* What a block the source gave no name declares is reached from that
       block and from nowhere else (LRM 9.3.4). */
    if (vpi_handle_by_name((PLI_BYTE8*)"Top.deep", NULL) != NULL)
      differences += Differ("Top.deep is reached from outside its block");
    vpiHandle blocks = vpi_iterate(vpiInternalScope, top);
    int deep = 0;
    for (vpiHandle h = blocks == NULL ? NULL : vpi_scan(blocks); h != NULL;
         h = vpi_scan(blocks)) {
      if (vpi_get(vpiType, h) != vpiBegin) continue;
      vpiHandle found = vpi_handle_by_name((PLI_BYTE8*)"deep", h);
      if (found != NULL) {
        ++deep;
        const char* name = vpi_get_str(vpiName, found);
        if (name == NULL || strcmp(name, "deep") != 0)
          differences += Differ("what deep names is not deep");
      }
    }
    if (deep != 1) differences += Differ("deep is not reached from its block");
  }

  const char* absent[] = {"Top.nothing",      "Top.u[2]", "Top.g[2]",
                          "Top.main.nothing", "Nothing",  "$unit::unit_task",
                          "unit_task",        "Top.pt"};
  for (size_t i = 0; i < sizeof absent / sizeof absent[0]; ++i) {
    if (vpi_handle_by_name((PLI_BYTE8*)absent[i], NULL) != NULL) {
      vpi_printf("difference: '%s' reaches something\n", absent[i]);
      ++differences;
    }
  }
  vpiHandle it = vpi_iterate(vpiInstance, NULL);
  for (vpiHandle h = it == NULL ? NULL : vpi_scan(it); h != NULL;
       h = vpi_scan(it)) {
    if (vpi_get(vpiType, h) == vpiPackage && vpi_get(vpiUnit, h) == 1 &&
        vpi_handle_by_name((PLI_BYTE8*)"unit_task", h) != NULL) {
      vpi_printf(
          "difference: the compilation unit's task is reached by name\n");
      ++differences;
    }
  }
  return differences;
}
