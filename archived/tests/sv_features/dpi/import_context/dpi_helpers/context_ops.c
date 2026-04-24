#include <svdpi.h>
#include <string.h>

// Test 1: module-scope context import -- checks ABI param, runtime query,
// and actual instance identity via scope path API.
int get_scope_info(svScope scope) {
  if (scope == NULL) return 0;
  svScope current = svGetScope();
  if (current != scope) return 0;
  const char* name = svGetNameFromScope(scope);
  if (name == NULL) return 0;
  if (strcmp(name, "Test") != 0) return 0;
  return 1;
}

// Test 2: non-module context (package scope) -- null scope.
// Representative non-module context case. The implementation rule is broader:
// any call site with no instance_ptr must pass and install null scope.
int get_scope_from_pkg(svScope scope) {
  if (scope != NULL) return 0;
  svScope current = svGetScope();
  if (current != NULL) return 0;
  return 1;
}

// Test 3: scope restore -- first call corrupts scope, second call verifies
// compiler-generated pop restored it.
void corrupt_scope(svScope scope) {
  (void)scope;
  svSetScope(NULL);
}

int check_scope_restored(svScope scope) {
  if (scope == NULL) return 0;
  svScope current = svGetScope();
  if (current != scope) return 0;
  return 1;
}

// Test 4: non-context import -- arity-sensitive regression.
// If a hidden scope param were accidentally prepended, a would be the scope
// pointer and this would not return 102.
int add_pair(int a, int b) {
  return a * 100 + b;
}
