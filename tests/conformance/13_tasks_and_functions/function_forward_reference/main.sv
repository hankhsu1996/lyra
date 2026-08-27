// A task or function name is resolved by the upwards hierarchical name rules
// rather than by position in the text, so a call may name a subroutine that is
// declared further down the same scope (LRM 13.7).
module Top;
  int result;

  function automatic int caller(int n);
    return callee(n) + 1;
  endfunction

  function automatic int callee(int n);
    return n * 2;
  endfunction

  initial begin
    result = caller(10);
  end

  final begin
    if (result !== 21) $fatal(1, "result was %0d, expected 21", result);
    $display("All checks passed");
  end
endmodule
