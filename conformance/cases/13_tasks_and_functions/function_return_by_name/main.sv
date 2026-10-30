// A function definition implicitly declares a variable of the return type
// bearing the function's own name, and assigning to that name is the second
// way to specify the return value. The name can be read back inside the body,
// and a return statement overrides whatever was assigned to it (LRM 13.4.1).
module Top;
  int squared;
  int high;
  int low;

  function automatic int square(input int n);
    square = n * n;
  endfunction

  function automatic int clamp(input int v);
    clamp = v;
    if (v > 10) return 10;
    clamp = clamp + 100;
  endfunction

  initial begin
    squared = square(6);
    high = clamp(50);
    low = clamp(3);
  end

  final begin
    if (squared !== 36) $fatal(1, "squared was %0d, expected 36", squared);
    if (high !== 10) $fatal(1, "high was %0d, expected 10", high);
    if (low !== 103) $fatal(1, "low was %0d, expected 103", low);
    $display("All checks passed");
  end
endmodule
