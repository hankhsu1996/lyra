// An automatic function allocates its formal arguments and local variables for
// each call and initializes them on entry, which makes it reentrant: a call
// already in progress keeps its own values while a nested call to the same
// function runs, two functions may call each other down to a base case, and a
// local never carries a value left by an earlier call (LRM 13.4.2, 6.21).
module Top;
  int factorial_5;
  int first_call;
  int second_call;
  int even;
  int odd;

  function automatic int fact(int n);
    if (n <= 1) return 1;
    return n * fact(n - 1);
  endfunction

  function automatic int fresh();
    int x;
    x = x + 5;
    return x;
  endfunction

  function automatic int is_even(int n);
    if (n == 0) return 1;
    return is_odd(n - 1);
  endfunction

  function automatic int is_odd(int n);
    if (n == 0) return 0;
    return is_even(n - 1);
  endfunction

  initial begin
    factorial_5 = fact(5);

    first_call = fresh();
    second_call = fresh();

    even = is_even(8);
    odd = is_odd(8);
  end

  final begin
    if (factorial_5 !== 120)
      $fatal(1, "factorial_5 was %0d, expected 120", factorial_5);
    if (first_call !== 5)
      $fatal(1, "first_call was %0d, expected 5", first_call);
    if (second_call !== 5)
      $fatal(1, "second_call was %0d, expected 5", second_call);
    if (even !== 1) $fatal(1, "even was %0d, expected 1", even);
    if (odd !== 0) $fatal(1, "odd was %0d, expected 0", odd);
    $display("All checks passed");
  end
endmodule
