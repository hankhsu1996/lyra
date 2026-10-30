// A return statement gives a function its value and exits the function at that
// point, so a body holding several returns yields the value of the one it
// reaches and runs nothing after it. Between the header and endfunction a body
// may declare its own variables and run any number of statements
// (LRM 12.8, 13.4.1).
module Top;
  int constant_value;
  int through_local;
  int positive;
  int zero;
  int negative;

  function automatic int get_value();
    return 42;
  endfunction

  function automatic int compute(int x);
    int temp;
    temp = x + 10;
    return temp * 2;
  endfunction

  function automatic int sign(int x);
    if (x < 0) return -1;
    if (x == 0) return 0;
    return 1;
  endfunction

  initial begin
    zero = 9;

    constant_value = get_value();
    through_local = compute(11);
    positive = sign(5);
    zero = sign(0);
    negative = sign(-3);
  end

  final begin
    if (constant_value !== 42)
      $fatal(1, "constant_value was %0d, expected 42", constant_value);
    if (through_local !== 42)
      $fatal(1, "through_local was %0d, expected 42", through_local);
    if (positive !== 1) $fatal(1, "positive was %0d, expected 1", positive);
    if (zero !== 0) $fatal(1, "zero was %0d, expected 0", zero);
    if (negative !== -1) $fatal(1, "negative was %0d, expected -1", negative);
    $display("All checks passed");
  end
endmodule
