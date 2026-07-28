`timescale 1ns / 1ps
// LRM 35.4: an imported subroutine resolves to a program-global symbol in a
// name space of its own, separate from every compilation-unit scope name space.
// So where the declaration was written is a name-resolution fact only: an import
// declared in a package (LRM 26.3) or at `$unit` scope (LRM 3.12.1) is called
// exactly as one declared in the calling module, from the calling module and
// from the declaring package's own function alike.
//
// LRM 35.5.3 pins the one thing about an import that is not global: a `context`
// import observes the instantiated scope of its declaration. A package and
// `$unit` are never instantiated, so such an import observes no scope -- it can
// still reach a receiver-less export directly, since that export needs no
// instance, which is what `probe` proves by seeing a null scope and calling
// `pkg_double` anyway.
package dpi_pkg;
  import "DPI-C" function int scale(input int a);
  import "DPI-C" context function int probe(input int a);
  import "DPI-C" function void split(input int a, output int lo,
                                     output int hi);

  export "DPI-C" function pkg_double;
  function automatic int pkg_double(input int a);
    return a * 2;
  endfunction

  // The declaring unit calls its own import, so it holds the same record the
  // calling module does.
  function automatic int via_pkg(input int a);
    return scale(a);
  endfunction
endpackage

import "DPI-C" function int negate(input int a);

module Top;
  import dpi_pkg::*;
  int from_module;
  int from_pkg_fn;
  int from_unit;
  int with_context;
  int lo;
  int hi;
  initial begin
    from_module = scale(21);
    from_pkg_fn = via_pkg(5);
    from_unit = negate(7);
    with_context = probe(4);
    split(605, lo, hi);
  end
endmodule
