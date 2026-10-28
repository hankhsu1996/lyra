// The standard orders no package's variable initializers against another's
// (LRM 26.2, 26.3), so an initializer reading another package's variable may
// see that variable initialized or still at its default. Lyra initializes the
// package read first, so such an initializer sees its value.
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
