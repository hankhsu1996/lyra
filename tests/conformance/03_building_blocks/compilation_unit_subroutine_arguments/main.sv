// A task or function may be declared in the compilation-unit scope, which
// holds any item a package may hold, and a design element in that unit calls it
// by its simple name (LRM 3.12.1, 26.2). Its formals bind by the rules that
// govern any subroutine call: an output formal is copied back to the caller's
// variable when the call returns, and a ref formal shares that variable, so a
// write the callee makes after suspending lands in it (LRM 13.5, 13.5.2).
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

  final begin
    if (b !== 6) $fatal(1, "b was %0d, expected 6", b);
    if (r !== 12) $fatal(1, "r was %0d, expected 12", r);
    $display("All checks passed");
  end
endmodule
