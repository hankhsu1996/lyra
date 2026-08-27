// An export declaration gives a SystemVerilog function a global foreign
// symbol, so foreign code already inside a DPI call chain can call it back.
// The declaration has to sit in the scope where the function itself is
// defined, and that scope may be a module, a package, the compilation unit, or
// a generate block (LRM 35.4, 35.7, Annex H.9.2). Every exported function is a
// context function, and the one a foreign routine may call without first
// choosing a scope is an export of the scope its own import declaration sits
// in (LRM 35.5.3, 35.7).
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

  export "DPI-C" function module_double;
  function int module_double(input int x);
    return x * 2;
  endfunction

  import "DPI-C" context function int call_module(input int x);

  int from_module;
  int from_package;
  int from_unit;
  int from_block;

  initial begin
    // Each answer carries both the exported function's result and the
    // argument that produced it, so a chain that never reached the export
    // would report the argument alone.
    from_module = call_module(7);
    from_package = call_pkg(7);
    from_unit = call_unit(7);
  end

  if (1) begin : block
    export "DPI-C" function block_quintuple;
    function int block_quintuple(input int x);
      return x * 5;
    endfunction

    import "DPI-C" context function int call_block(input int x);

    initial from_block = call_block(7);
  end

  final begin
    if (from_module !== 147)
      $fatal(1, "the module-scope export answered %0d, expected 147",
             from_module);
    if (from_package !== 217)
      $fatal(1, "the package-scope export answered %0d, expected 217",
             from_package);
    if (from_unit !== 287)
      $fatal(1, "the unit-scope export answered %0d, expected 287",
             from_unit);
    if (from_block !== 357)
      $fatal(1, "the generate-block export answered %0d, expected 357",
             from_block);
    $display("All checks passed");
  end
endmodule
