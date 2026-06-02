module Top;
  int r_fact;
  int r_reinit1;
  int r_reinit2;
  int r_forward;
  int r_even;
  int r_odd;

  function automatic int fact(int n);
    if (n <= 1) return 1;
    return n * fact(n - 1);
  endfunction

  function automatic int g();
    int x;
    x = x + 5;
    return x;
  endfunction

  function automatic int caller(int n);
    return callee(n) + 1;
  endfunction

  function automatic int callee(int n);
    return n * 2;
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
    r_fact = fact(5);
    r_reinit1 = g();
    r_reinit2 = g();
    r_forward = caller(10);
    r_even = is_even(8);
    r_odd = is_odd(8);
  end
endmodule
