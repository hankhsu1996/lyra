// A ref argument is not copied into the subroutine: caller and subroutine
// share the one variable, so the formal is a target for every assignment form
// the actual accepts -- plain, compound, and part-select -- and each write
// lands in the caller's variable. A const ref shares the variable read-only,
// leaving the actual as it was. Which variable is shared is not restricted to
// one the design declares: a subroutine's own automatic variable is a variable
// like any other, so a nested call writing through its formal writes the
// enclosing call's local. ref is its own direction and may sit beside input and
// output formals in one argument list (LRM 13.5.2).
module Top;
  int a;
  int b;
  int src;
  int doubled_src;
  int bumped;
  int bump_ret;
  int compound;
  int partial;
  int out_arg;
  int ref_arg;
  int lent_local;

  function automatic void swap(ref int x, ref int y);
    int t;
    t = x;
    x = y;
    y = t;
  endfunction

  function automatic int doubled(const ref int d);
    return d * 2;
  endfunction

  function automatic int bump_get(ref int x);
    x = x + 1;
    return x;
  endfunction

  function automatic void add_into(ref int x);
    x += 7;
  endfunction

  function automatic void set_low_nibble(ref int x);
    x[3:0] = 4'hf;
  endfunction

  function automatic void mixed(output int o, ref int r, input int v);
    o = v * 2;
    r = r + v;
  endfunction

  function automatic int lend_own_local();
    int t;
    t = 4;
    add_into(t);
    return t + doubled(t);
  endfunction

  initial begin
    a = 3;
    b = 7;
    swap(a, b);

    src = 21;
    doubled_src = doubled(src);

    bumped = 5;
    bump_ret = bump_get(bumped) + 100;

    compound = 10;
    add_into(compound);

    partial = 0;
    set_low_nibble(partial);

    ref_arg = 100;
    mixed(out_arg, ref_arg, 5);

    lent_local = lend_own_local();
  end

  final begin
    if (a !== 7) $fatal(1, "a was %0d, expected 7", a);
    if (b !== 3) $fatal(1, "b was %0d, expected 3", b);
    if (doubled_src !== 42)
      $fatal(1, "doubled_src was %0d, expected 42", doubled_src);
    if (src !== 21) $fatal(1, "src was %0d, expected 21", src);
    if (bumped !== 6) $fatal(1, "bumped was %0d, expected 6", bumped);
    if (bump_ret !== 106)
      $fatal(1, "bump_ret was %0d, expected 106", bump_ret);
    if (compound !== 17) $fatal(1, "compound was %0d, expected 17", compound);
    if (partial !== 15) $fatal(1, "partial was %0d, expected 15", partial);
    if (out_arg !== 10) $fatal(1, "out_arg was %0d, expected 10", out_arg);
    if (ref_arg !== 105) $fatal(1, "ref_arg was %0d, expected 105", ref_arg);
    if (lent_local !== 33)
      $fatal(1, "lent_local was %0d, expected 33", lent_local);
    $display("All checks passed");
  end
endmodule
