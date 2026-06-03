module Top;
  int a;
  int b;
  int src;
  int ro;
  int e;
  int e_ret;

  // Two refs aliasing distinct cells, mutated through a void function.
  function automatic void swap(ref int x, ref int y);
    int t;
    t = x;
    x = y;
    y = t;
  endfunction

  // const ref: a read-only alias; the actual is not modified.
  function automatic int doubled(const ref int d);
    return d * 2;
  endfunction

  // A ref formal written then read; the call below uses it as a nested operand,
  // exercising a ref call in expression position (not statement position).
  function automatic int bump_get(ref int x);
    x = x + 1;
    return x;
  endfunction

  initial begin
    a = 3;
    b = 7;
    swap(a, b);

    src = 21;
    ro = doubled(src);

    e = 5;
    e_ret = bump_get(e) + 100;
  end
endmodule
