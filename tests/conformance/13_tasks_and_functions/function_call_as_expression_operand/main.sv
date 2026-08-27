// A call to a nonvoid function is an operand whose value is the value the
// function returned, so several calls combine in one expression and a function
// body may call another function inside the expression it returns
// (LRM 13.2, 13.5).
module Top;
  int combined;
  int nested;

  function automatic int square(int x);
    return x * x;
  endfunction

  function automatic int inner(int x);
    return x * 2;
  endfunction

  function automatic int outer(int x);
    return inner(x) + 10;
  endfunction

  initial begin
    combined = square(3) + square(4);
    nested = outer(5);
  end

  final begin
    if (combined !== 25) $fatal(1, "combined was %0d, expected 25", combined);
    if (nested !== 20) $fatal(1, "nested was %0d, expected 20", nested);
    $display("All checks passed");
  end
endmodule
