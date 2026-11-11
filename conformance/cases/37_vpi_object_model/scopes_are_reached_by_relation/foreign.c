/* Walks the design of `main.sv` by relation and counts every answer that is not
   the one stated here. Only what the standard settles is checked: a relation
   that some reading could widen is searched for the object it must hold rather
   than counted. */
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <sv_vpi_user.h>

static void Differ(int* differences, const char* what) {
  vpi_printf("difference: %s\n", what);
  ++*differences;
}

static void ExpectInt(int* d, PLI_INT32 got, PLI_INT32 want, const char* what) {
  if (got != want) {
    char line[256];
    snprintf(
        line, sizeof line, "%s is %d, expected %d", what, (int)got, (int)want);
    Differ(d, line);
  }
}

static void ExpectStr(
    int* d, const char* got, const char* want, const char* what) {
  if (got == NULL || strcmp(got, want) != 0) {
    char line[256];
    snprintf(
        line, sizeof line, "%s is '%s', expected '%s'", what,
        got == NULL ? "(null)" : got, want);
    Differ(d, line);
  }
}

/* The object of `type` related to `ref` whose name is `name`, or NULL. The
   iterator is released where the search stops before its end. */
static vpiHandle Find(PLI_INT32 type, vpiHandle ref, const char* name) {
  vpiHandle it = vpi_iterate(type, ref);
  if (it == NULL) return NULL;
  for (vpiHandle h = vpi_scan(it); h != NULL; h = vpi_scan(it)) {
    const char* got = vpi_get_str(vpiName, h);
    if (got != NULL && strcmp(got, name) == 0) {
      vpi_release_handle(it);
      return h;
    }
    vpi_release_handle(h);
  }
  return NULL;
}

/* The object of `type` related to `ref` whose own type is `kind`. */
static vpiHandle FindOfType(PLI_INT32 type, vpiHandle ref, PLI_INT32 kind) {
  vpiHandle it = vpi_iterate(type, ref);
  if (it == NULL) return NULL;
  for (vpiHandle h = vpi_scan(it); h != NULL; h = vpi_scan(it)) {
    if (vpi_get(vpiType, h) == kind) {
      vpi_release_handle(it);
      return h;
    }
    vpi_release_handle(h);
  }
  return NULL;
}

static int Count(PLI_INT32 type, vpiHandle ref) {
  vpiHandle it = vpi_iterate(type, ref);
  int n = 0;
  if (it == NULL) return 0;
  for (vpiHandle h = vpi_scan(it); h != NULL; h = vpi_scan(it)) {
    ++n;
    vpi_release_handle(h);
  }
  return n;
}

static vpiHandle Expect(int* d, vpiHandle h, const char* what) {
  if (h == NULL) Differ(d, what);
  return h;
}

static void ExpectType(int* d, vpiHandle h, PLI_INT32 type, const char* full) {
  if (h == NULL) return;
  ExpectInt(d, vpi_get(vpiType, h), type, full);
  ExpectStr(d, vpi_get_str(vpiFullName, h), full, "the full name");
}

static void Tops(int* d) {
  vpiHandle top = Expect(d, Find(vpiModule, NULL, "Top"), "no top module Top");
  ExpectType(d, top, vpiModule, "Top");
  if (top != NULL)
    ExpectInt(d, vpi_get(vpiTopModule, top), 1, "Top's vpiTopModule");
  /* Leaf, Bus and Prog are instantiated, so no top module bears their name. */
  if (Find(vpiModule, NULL, "Leaf") != NULL) Differ(d, "Leaf is a top module");
  vpiHandle pkg = Expect(
      d, Find(vpiInstance, NULL, "P"), "no package P among the instances");
  ExpectType(d, pkg, vpiPackage, "P::");
  if (pkg != NULL) {
    vpiHandle pt =
        Expect(d, Find(vpiInternalScope, pkg, "pt"), "no task P::pt");
    ExpectType(d, pt, vpiTask, "P::pt");
    if (pt != NULL) {
      vpiHandle in =
          Expect(d, vpi_handle(vpiInstance, pt), "P::pt has no instance");
      if (in != NULL)
        ExpectInt(d, vpi_compare_objects(in, pkg), 1, "P::pt's instance");
    }
  }
  if (Find(vpiInstance, NULL, "Top") == NULL)
    Differ(d, "Top is not among the instances");
}

static void Loops(int* d, vpiHandle one, vpiHandle three) {
  vpiHandle g1 = Expect(d, Find(vpiGenScopeArray, one, "g"), "no Top.one.g");
  vpiHandle g3 =
      Expect(d, Find(vpiGenScopeArray, three, "g"), "no Top.three.g");
  ExpectType(d, g1, vpiGenScopeArray, "Top.one.g");
  if (g1 != NULL)
    ExpectInt(d, vpi_get(vpiSize, g1), 1, "the size of Top.one.g");
  if (g3 != NULL) {
    ExpectInt(d, vpi_get(vpiSize, g3), 3, "the size of Top.three.g");
    ExpectInt(d, Count(vpiGenScope, g3), 3, "the scopes of Top.three.g");
    vpiHandle e = Expect(d, vpi_handle_by_index(g3, 2), "no Top.three.g[2]");
    ExpectType(d, e, vpiGenScope, "Top.three.g[2]");
    if (e != NULL) {
      ExpectInt(d, vpi_get(vpiArrayMember, e), 1, "g[2]'s vpiArrayMember");
      ExpectStr(d, vpi_get_str(vpiName, e), "g[2]", "g[2]'s name");
    }
    if (vpi_handle_by_index(g3, 3) != NULL) Differ(d, "Top.three.g[3] exists");
  }
  if (Find(vpiInternalScope, one, "wide") != NULL)
    Differ(d, "Top.one built its arm wide");
  vpiHandle wide = Expect(
      d, Find(vpiInternalScope, three, "wide"),
      "Top.three did not build its arm wide");
  ExpectType(d, wide, vpiGenScope, "Top.three.wide");
  if (wide != NULL) {
    ExpectInt(d, vpi_get(vpiImplicitDecl, wide), 0, "wide's vpiImplicitDecl");
    ExpectInt(d, vpi_get(vpiArrayMember, wide), 0, "wide's vpiArrayMember");
  }
}

static void Arrays(int* d, vpiHandle top) {
  vpiHandle grid =
      Expect(d, Find(vpiModuleArray, top, "grid"), "no module array Top.grid");
  ExpectType(d, grid, vpiModuleArray, "Top.grid");
  if (grid != NULL) {
    ExpectInt(d, vpi_get(vpiSize, grid), 6, "the size of Top.grid");
    ExpectInt(d, Count(vpiModule, grid), 6, "the modules of Top.grid");
    PLI_INT32 at[2] = {1, 2};
    vpiHandle e =
        Expect(d, vpi_handle_by_multi_index(grid, 2, at), "no Top.grid[1][2]");
    ExpectType(d, e, vpiModule, "Top.grid[1][2]");
    if (e != NULL) {
      ExpectInt(
          d, vpi_get(vpiArrayMember, e), 1, "grid[1][2]'s vpiArrayMember");
      ExpectType(
          d, Expect(d, vpi_handle(vpiModule, e), "grid has no module"),
          vpiModule, "Top");
      ExpectType(
          d, Expect(d, vpi_handle(vpiInstance, e), "grid has no instance"),
          vpiModule, "Top");
    }
    PLI_INT32 outside[2] = {2, 0};
    if (vpi_handle_by_multi_index(grid, 2, outside) != NULL)
      Differ(d, "Top.grid[2][0] exists");
  }
  vpiHandle buses = Expect(
      d, Find(vpiInterfaceArray, top, "buses"), "no interface array Top.buses");
  ExpectType(d, buses, vpiInterfaceArray, "Top.buses");
  if (buses != NULL) {
    ExpectInt(d, vpi_get(vpiSize, buses), 2, "the size of Top.buses");
    ExpectType(
        d, Expect(d, vpi_handle_by_index(buses, 1), "no Top.buses[1]"),
        vpiInterface, "Top.buses[1]");
  }
  ExpectType(
      d, Expect(d, Find(vpiInterface, top, "bus"), "no interface Top.bus"),
      vpiInterface, "Top.bus");
  ExpectType(
      d, Expect(d, Find(vpiProgram, top, "prog"), "no program Top.prog"),
      vpiProgram, "Top.prog");
}

static void Instances(int* d, vpiHandle top) {
  vpiHandle one = Expect(d, Find(vpiModule, top, "one"), "no module Top.one");
  ExpectType(d, one, vpiModule, "Top.one");
  vpiHandle three =
      Expect(d, Find(vpiModule, top, "three"), "no module Top.three");
  ExpectType(d, three, vpiModule, "Top.three");
  if (one != NULL && three != NULL) Loops(d, one, three);
  Arrays(d, top);
}

static void Generates(int* d, vpiHandle top) {
  vpiHandle blk =
      Expect(d, Find(vpiInternalScope, top, "genblk1"), "no scope Top.genblk1");
  ExpectType(d, blk, vpiGenScope, "Top.genblk1");
  if (blk != NULL) {
    ExpectInt(d, vpi_get(vpiImplicitDecl, blk), 1, "genblk1's vpiImplicitDecl");
    vpiHandle leaf = Expect(
        d, Find(vpiModule, blk, "in_unnamed"), "no Top.genblk1.in_unnamed");
    ExpectType(d, leaf, vpiModule, "Top.genblk1.in_unnamed");
    if (leaf != NULL) {
      ExpectType(
          d,
          Expect(
              d, vpi_handle(vpiModule, leaf),
              "in_unnamed has no module above it"),
          vpiModule, "Top");
    }
  }
  /* The unnamed loop is Top's fourth generate construct (LRM 27.6), and each
     block it builds is an implicit scope (LRM 37.85 detail 2). */
  vpiHandle loop = Expect(
      d, Find(vpiGenScopeArray, top, "genblk4"),
      "no generate scope array Top.genblk4");
  ExpectType(d, loop, vpiGenScopeArray, "Top.genblk4");
  if (loop != NULL) {
    vpiHandle first =
        Expect(d, vpi_handle_by_index(loop, 0), "no Top.genblk4[0]");
    if (first != NULL)
      ExpectInt(
          d, vpi_get(vpiImplicitDecl, first), 1,
          "genblk4[0]'s vpiImplicitDecl");
  }
  vpiHandle none = Expect(
      d, Find(vpiGenScopeArray, top, "none"),
      "no generate scope array Top.none");
  ExpectType(d, none, vpiGenScopeArray, "Top.none");
  if (none != NULL) {
    ExpectInt(d, vpi_get(vpiSize, none), 0, "the size of Top.none");
    if (vpi_iterate(vpiGenScope, none) != NULL)
      Differ(d, "Top.none holds a scope");
  }
}

/* One declaration in two blocks a loop built is two objects. */
static void OneTaskPerBlock(int* d, vpiHandle top) {
  vpiHandle tg = Expect(d, Find(vpiGenScopeArray, top, "tg"), "no Top.tg");
  if (tg == NULL) return;
  vpiHandle first = vpi_handle_by_index(tg, 0);
  vpiHandle second = vpi_handle_by_index(tg, 1);
  if (first == NULL || second == NULL) {
    Differ(d, "Top.tg[0] or Top.tg[1] is not reached");
    return;
  }
  vpiHandle t0 =
      Expect(d, Find(vpiInternalScope, first, "t"), "no Top.tg[0].t");
  vpiHandle t1 =
      Expect(d, Find(vpiInternalScope, second, "t"), "no Top.tg[1].t");
  ExpectType(d, t0, vpiTask, "Top.tg[0].t");
  ExpectType(d, t1, vpiTask, "Top.tg[1].t");
  if (t0 != NULL && t1 != NULL)
    ExpectInt(
        d, vpi_compare_objects(t0, t1), 0, "the task of two blocks compared");
}

static void Declared(int* d, vpiHandle top) {
  ExpectType(
      d, Expect(d, Find(vpiInternalScope, top, "t"), "no task Top.t"), vpiTask,
      "Top.t");
  ExpectType(
      d, Expect(d, Find(vpiInternalScope, top, "f"), "no function Top.f"),
      vpiFunction, "Top.f");
  ExpectType(
      d, Expect(d, Find(vpiInternalScope, top, "walk"), "no function Top.walk"),
      vpiFunction, "Top.walk");
  ExpectType(
      d, Expect(d, Find(vpiInternalScope, top, "C"), "no class Top.C"),
      vpiClassDefn, "Top.C");
  ExpectType(
      d, Expect(d, Find(vpiInternalScope, top, "cb"), "no clocking Top.cb"),
      vpiClockingBlock, "Top.cb");
  vpiHandle t = Find(vpiInternalScope, top, "t");
  if (t != NULL) {
    ExpectType(
        d,
        Expect(
            d, Find(vpiInternalScope, t, "in_task"), "no block Top.t.in_task"),
        vpiNamedBegin, "Top.t.in_task");
    ExpectType(
        d, Expect(d, vpi_handle(vpiInstance, t), "Top.t has no instance"),
        vpiModule, "Top");
  }
  /* An instance's tasks and functions, and its classes, are its own
     relations (LRM 37.10), a function it imports from C as much as one with a
     body (LRM 37.41 detail 6), and a class is one class defn however many of
     its specializations the design uses (LRM 37.31). */
  ExpectInt(d, Count(vpiTaskFunc, top), 3, "Top's tasks and functions");
  if (Find(vpiClassDefn, top, "C") == NULL)
    Differ(d, "C is not among Top's class defns");
  int named_g = 0;
  vpiHandle it = vpi_iterate(vpiClassDefn, top);
  for (vpiHandle h = it == NULL ? NULL : vpi_scan(it); h != NULL;
       h = vpi_scan(it)) {
    const char* name = vpi_get_str(vpiName, h);
    if (name != NULL && strcmp(name, "G") == 0) ++named_g;
  }
  ExpectInt(d, named_g, 1, "the class defns named G");
}

static void Blocks(int* d, vpiHandle top) {
  vpiHandle main_block =
      Expect(d, Find(vpiInternalScope, top, "main"), "no block Top.main");
  ExpectType(d, main_block, vpiNamedBegin, "Top.main");
  if (main_block == NULL) return;
  ExpectInt(
      d, Count(vpiInternalScope, main_block), 9, "the scopes inside Top.main");
  /* A label on a `for` declaring no variable names a block around it (LRM
     9.3.5), and an unnamed block declaring only a type is a scope (LRM 37.12
     detail 1). */
  ExpectType(
      d,
      Expect(
          d, Find(vpiInternalScope, main_block, "lbl"),
          "no block Top.main.lbl"),
      vpiNamedBegin, "Top.main.lbl");
  ExpectType(
      d,
      Expect(
          d, Find(vpiInternalScope, main_block, "inner"),
          "no block Top.main.inner"),
      vpiNamedBegin, "Top.main.inner");
  vpiHandle fa = Expect(
      d, Find(vpiInternalScope, main_block, "fa"), "no fork Top.main.fa");
  ExpectType(d, fa, vpiNamedFork, "Top.main.fa");
  if (fa != NULL)
    ExpectInt(d, vpi_get(vpiJoinType, fa), vpiJoinAny, "fa's join");
  vpiHandle fj = Expect(
      d, Find(vpiInternalScope, main_block, "fj"), "no fork Top.main.fj");
  if (fj != NULL) ExpectInt(d, vpi_get(vpiJoinType, fj), vpiJoin, "fj's join");
  vpiHandle fn = Expect(
      d, Find(vpiInternalScope, main_block, "fn"), "no fork Top.main.fn");
  if (fn != NULL)
    ExpectInt(d, vpi_get(vpiJoinType, fn), vpiJoinNone, "fn's join");
  if (FindOfType(vpiInternalScope, main_block, vpiForeachStmt) == NULL)
    Differ(d, "the foreach loop declaring its index is no scope");
  if (FindOfType(vpiInternalScope, main_block, vpiBegin) == NULL)
    Differ(d, "the unnamed block declaring a variable is no scope");
  if (FindOfType(vpiInternalScope, main_block, vpiFor) == NULL)
    Differ(d, "the for loop declaring its index is no scope");

  vpiHandle it = Expect(
      d, vpi_iterate(vpiInternalScope, main_block),
      "no iterator over Top.main");
  if (it != NULL) {
    ExpectInt(d, vpi_get(vpiType, it), vpiIterator, "an iterator's type");
    ExpectInt(
        d, vpi_get(vpiIteratorType, it), vpiInternalScope,
        "an iterator's vpiIteratorType");
    vpiHandle use = Expect(d, vpi_handle(vpiUse, it), "an iterator has no use");
    if (use != NULL)
      ExpectInt(
          d, vpi_compare_objects(use, main_block), 1,
          "an iterator's use is the object it was made on");
    ExpectInt(d, vpi_release_handle(it), 1, "releasing an unfinished iterator");
  }
  vpiHandle tops = vpi_iterate(vpiModule, NULL);
  if (tops != NULL) {
    if (vpi_handle(vpiUse, tops) != NULL)
      Differ(d, "an iterator made on no object has a use");
    vpi_release_handle(tops);
  }
}

static void Handles(int* d, vpiHandle top) {
  vpiHandle a = Find(vpiModule, top, "three");
  vpiHandle b = Find(vpiModule, top, "three");
  vpiHandle c = Find(vpiModule, top, "one");
  if (a == NULL || b == NULL || c == NULL) {
    Differ(d, "Top.three or Top.one could not be reached twice");
    return;
  }
  ExpectInt(d, vpi_compare_objects(a, b), 1, "two handles to Top.three");
  ExpectInt(d, vpi_compare_objects(a, c), 0, "Top.three against Top.one");
  ExpectInt(d, vpi_release_handle(a), 1, "releasing a handle");
  ExpectStr(
      d, vpi_get_str(vpiName, b), "three",
      "the other handle after one was released");
}

int walk(void) {
  int differences = 0;
  Tops(&differences);
  vpiHandle top = Find(vpiModule, NULL, "Top");
  if (top != NULL) {
    Instances(&differences, top);
    Generates(&differences, top);
    Declared(&differences, top);
    OneTaskPerBlock(&differences, top);
    Blocks(&differences, top);
    Handles(&differences, top);
  }
  return differences;
}
