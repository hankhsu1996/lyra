// An anonymous program declares its items in the package or compilation-unit
// scope around it, opening no scope of its own, and only programs may name
// them (LRM 24.6). A program reaches them by the scope's name, by import, and
// as `$unit` items; one calls another; a class declared in one is constructed
// from a program; and a task of one runs in the calling program's thread, so
// it waits and reads in the reactive region set (LRM 24.5, 24.3.1).
package pkg;
  localparam int Base = 10;

  function automatic int plus_base(int x);
    return x + Base;
  endfunction

  program;
    function automatic int thrice(int x);
      return 3 * x;
    endfunction

    function automatic int thrice_plus_base(int x);
      return thrice(x) + Base;
    endfunction

    task automatic read_after_edge(ref logic clk, ref logic q, output logic seen);
      @(posedge clk) seen = q;
    endtask

    class Box;
      int v;
      function new(int v);
        this.v = v;
      endfunction
      function int doubled();
        return 2 * v;
      endfunction
    endclass
  endprogram
endpackage

program;
  function automatic int negate(int x);
    return -x;
  endfunction
endprogram

module Top;
  logic clk = 0, q = 0;

  always @(posedge clk) q <= 1;
  initial #2 clk = 1;

  p check ();
endmodule

program p;
  import pkg::*;

  initial begin
    Box b;
    logic seen;
    if (pkg::thrice(2) != 6) $fatal(1, "pkg::thrice(2) was %0d", pkg::thrice(2));
    if (thrice(3) != 9) $fatal(1, "imported thrice(3) was %0d", thrice(3));
    if (thrice_plus_base(1) != 13)
      $fatal(1, "thrice_plus_base(1) was %0d", thrice_plus_base(1));
    if (plus_base(1) != 11) $fatal(1, "plus_base(1) was %0d", plus_base(1));
    if (negate(4) != -4) $fatal(1, "negate(4) was %0d", negate(4));
    if ($unit::negate(5) != -5) $fatal(1, "$unit::negate(5) was %0d", $unit::negate(5));
    b = new(7);
    if (b.doubled() != 14) $fatal(1, "b.doubled() was %0d", b.doubled());
    read_after_edge(Top.clk, Top.q, seen);
    if (seen !== 1) $fatal(1, "the task read q as %b before the NBA", seen);
    $display("All checks passed");
  end
endprogram
