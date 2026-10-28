// An export may be declared in a package or in the compilation unit
// (LRM 35.7), and a context import is supplied "the instantiated scope where
// the import declaration is located", whose own exports it may call directly
// (LRM 35.5.3). A package and the compilation unit are not instantiated, so the
// standard does not say which scope an import declared in one of them is
// supplied. Lyra supplies the package, or the compilation unit, the import is
// declared in, so such an import calls the exports declared beside it.
package math_pkg;
  export "DPI-C" function pkg_triple;
  function int pkg_triple(input int x);
    return x * 3;
  endfunction

  import "DPI-C" context function int call_pkg(input int x);
endpackage

export "DPI-C" function unit_quadruple;
function int unit_quadruple(input int x);
  return x * 4;
endfunction

import "DPI-C" context function int call_unit(input int x);

module Top;
  import math_pkg::*;

  int from_package;
  int from_unit;

  initial begin
    from_package = call_pkg(7);
    from_unit = call_unit(7);
  end

  final begin
    if (from_package !== 217)
      $fatal(1, "the package-scope export answered %0d, expected 217",
             from_package);
    if (from_unit !== 287)
      $fatal(1, "the unit-scope export answered %0d, expected 287",
             from_unit);
    $display("All checks passed");
  end
endmodule
