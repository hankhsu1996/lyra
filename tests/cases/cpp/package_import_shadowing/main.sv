// The LRM 26.3 name-search rules for a wildcard import. A wildcard `import pkg::*`
// makes a name a candidate only when the scope does not otherwise declare it: a
// local declaration of the same name shadows the import, and a reference to the
// shadowed name reaches the local, leaving the package's own cell untouched. A
// name the scope does not declare resolves through the wildcard to the package
// member. An import is also a package-to-package reference: a package that
// imports another package's variable and reads it in an initializer reaches the
// imported cell by name, the same cross-unit form a module uses.
package pkg;
  int shared = 5;
  int other = 9;
endpackage

package mid;
  import pkg::other;
  int derived = other + 1;
endpackage

module Top;
  import pkg::*;
  int shared;

  int local_val;
  int pkg_shared_after;
  int wildcard_val;
  int cross_pkg;

  initial begin
    shared = 3;
    local_val = shared;
    pkg_shared_after = pkg::shared;
    wildcard_val = other;
    cross_pkg = mid::derived;
  end
endmodule
