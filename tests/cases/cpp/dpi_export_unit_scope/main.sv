`timescale 1ns / 1ps
// LRM 35.7 $unit-scoped export: a subroutine declared at compilation-unit scope
// (LRM 3.12.1) has no instance and no receiver, exactly like a package function,
// so its foreign-linkage wrapper calls the free function and threads the run's
// effects. The context import reaches the exported `triple` through the
// import -> export chain; a result of 21 (7 tripled) proves the $unit export ran.
export "DPI-C" function triple;
function int triple(input int x);
  return x * 3;
endfunction

module Top;
  import "DPI-C" context function int call_unit(input int x);
  initial $display("unit=%0d", call_unit(7));
endmodule
