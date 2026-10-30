// A formal argument declared without a direction is an input, and an input
// argument is passed by value: the actual's value is copied into the formal at
// the call, so an assignment to the formal inside the body cannot reach the
// caller's variable (LRM 13.4, 13.5.1).
module Top;
  int given;
  int copied;
  int weighed;

  function automatic int twice(int n);
    n = n * 2;
    return n;
  endfunction

  function automatic int weigh(int a, int b);
    return a * 10 + b;
  endfunction

  initial begin
    given = 21;
    copied = twice(given);
    weighed = weigh(3, 7);
  end

  final begin
    if (copied !== 42) $fatal(1, "copied was %0d, expected 42", copied);
    if (given !== 21) $fatal(1, "given was %0d, expected 21", given);
    if (weighed !== 37) $fatal(1, "weighed was %0d, expected 37", weighed);
    $display("All checks passed");
  end
endmodule
