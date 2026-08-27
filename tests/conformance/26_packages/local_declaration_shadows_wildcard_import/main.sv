// A wildcard import offers a package's identifiers to a scope only where that
// scope does not otherwise declare them (LRM 26.3). A name the scope declares
// itself is the one a direct reference finds, and the package's identifier of
// the same spelling stays reachable by its resolved name and is untouched by
// writes to the local declaration; a name the scope does not declare resolves
// through the wildcard to the package's (LRM 26.5).
package pkg;
  int shared = 5;
  int other = 9;
endpackage

module Top;
  import pkg::*;
  int shared;

  int local_val;
  int pkg_shared_after;
  int wildcard_val;

  initial begin
    shared = 3;
    local_val = shared;
    pkg_shared_after = pkg::shared;
    wildcard_val = other;
  end

  final begin
    if (local_val !== 3) $fatal(1, "local_val was %0d, expected 3", local_val);
    if (pkg_shared_after !== 5)
      $fatal(1, "pkg_shared_after was %0d, expected 5", pkg_shared_after);
    if (wildcard_val !== 9)
      $fatal(1, "wildcard_val was %0d, expected 9", wildcard_val);
    $display("All checks passed");
  end
endmodule
