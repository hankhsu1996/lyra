// An imported subroutine resolves to a global symbol in a name space of its
// own, separate from every compilation-unit scope name space, so where its
// import declaration was written decides only which SystemVerilog code can see
// the name (LRM 35.4, 35.5.4). A package and the compilation unit are both
// places such a declaration may sit, so one written in a package is reached
// from an importing module exactly as from the package's own function, and one
// written at compilation-unit scope is reached from a module in the same file
// (LRM Annex H.9.2).
//
// A context import is the one part of this that is not global: it observes the
// scope its declaration sits in rather than its call site, and a subroutine
// exported from that same scope is the one it may call back without setting a
// scope first (LRM 35.5.3). Both declarations below sit in the package, so the
// call back is legal wherever the import itself was called from.
package scaling_pkg;
  import "DPI-C" function int scale(input int a, input int b);
  import "DPI-C" context function int probe(input int a);
  import "DPI-C" function void split(
      input int a, output int lo, output int hi);

  export "DPI-C" function pkg_double;
  function automatic int pkg_double(input int a);
    return a * 2;
  endfunction

  function automatic int via_pkg(input int a, input int b);
    return scale(a, b);
  endfunction
endpackage

import "DPI-C" function int negate(input int a);

module Top;
  import scaling_pkg::*;

  int from_module;
  int from_declaring_package;
  int from_unit_scope;
  int through_export;
  int lo;
  int hi;

  initial begin
    from_module = scale(21, 5);
    from_declaring_package = via_pkg(4, 3);
    from_unit_scope = negate(7);
    through_export = probe(4);

    lo = -1;
    hi = -2;
    split(605, lo, hi);
  end

  final begin
    if (from_module !== 215)
      $fatal(1, "from_module was %0d, expected 215", from_module);
    if (from_declaring_package !== 43)
      $fatal(
          1, "from_declaring_package was %0d, expected 43",
          from_declaring_package);
    if (from_unit_scope !== -7)
      $fatal(1, "from_unit_scope was %0d, expected -7", from_unit_scope);
    if (through_export !== 804)
      $fatal(1, "through_export was %0d, expected 804", through_export);
    if (lo !== 5) $fatal(1, "lo was %0d, expected 5", lo);
    if (hi !== 6) $fatal(1, "hi was %0d, expected 6", hi);
    $display("All checks passed");
  end
endmodule
