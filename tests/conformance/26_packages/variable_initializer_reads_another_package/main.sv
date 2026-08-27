// A package's declarations may be referenced within another package, by a
// resolved name or by an import, and the compilation of the referenced package
// precedes the compilation of the scope importing it (LRM 26.2, 26.3). Since
// every package's variable declaration assignments happen before any initial or
// always procedure is started (LRM 26.2), a variable whose initializer reads
// another package's variable observes that variable already initialized rather
// than at its default value.
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
