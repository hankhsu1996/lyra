// A package's declarations may be referenced within another package, by a
// resolved name or by an import (LRM 26.2, 26.3). Every package's variable
// declaration assignments happen before any initial or always procedure is
// started (LRM 26.2), so both packages are initialized by the time the values
// below are read.
//
// The values themselves pin more than the standard fixes. LRM 26.2 states a
// barrier and orders no package's initializers against another's, and LRM 26.3
// orders compilation rather than execution, so a conforming simulator may run
// these in either order and answer 1 where this expects 11. What this case
// therefore checks is the order Lyra chooses: an initializer reading another
// package's variable sees that package's value, which is what every
// established simulator answers and what the code was written expecting.
package base_pkg;
  int seed = 10;
  int offset = 9;
endpackage

package derived_pkg;
  import base_pkg::offset;

  int from_resolved_name = base_pkg::seed + 1;
  int from_import = offset + 1;
endpackage

module Top;
  int resolved_read;
  int imported_read;

  initial begin
    resolved_read = derived_pkg::from_resolved_name;
    imported_read = derived_pkg::from_import;
  end

  final begin
    if (resolved_read !== 11)
      $fatal(1, "resolved_read was %0d, expected 11", resolved_read);
    if (imported_read !== 10)
      $fatal(1, "imported_read was %0d, expected 10", imported_read);
    $display("All checks passed");
  end
endmodule
