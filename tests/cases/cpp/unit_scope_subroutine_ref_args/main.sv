// A `$unit`-scope subroutine (LRM 3.12.1) reached from a design element carries
// its by-reference formals back exactly as a package subroutine does: the
// anonymous compilation unit shares the same cross-unit marshalling as a named
// package. An output written back and a ref aliased across a task suspension
// both cross the anonymous-unit boundary.
function automatic void unit_out(input int a, output int b);
  b = a + 1;
endfunction

task automatic unit_ref_scale(ref int r);
  #1;
  r = r * 3;
endtask

module Top;
  int b;
  int r;

  initial begin
    unit_out(5, b);
    r = 4;
    unit_ref_scale(r);
  end
endmodule
