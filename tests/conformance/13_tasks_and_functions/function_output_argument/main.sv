// An output argument carries a value out of a subroutine and nothing in. The
// formal is a variable of its own, initialized on entry to an automatic
// subroutine to its type's default initial value rather than to the actual's,
// and the actual receives the formal's value only when the call returns --
// which is why a write the body makes to the actual under its own name is
// overwritten by that copy. A function may have output arguments alongside its
// return value (LRM 13.4, 13.5, 6.21, 6.8).
module Top;
  int scalar;
  int first;
  int second;
  int quotient;
  int remainder;
  int aliased;
  logic [7:0] observed;
  bit started_unknown;

  function automatic void get_five(output int v);
    v = 5;
  endfunction

  function automatic void two(output int p, output int q);
    p = 3;
    q = 7;
  endfunction

  function automatic int divide(input int a, output int leftover);
    leftover = a % 3;
    return a / 3;
  endfunction

  function automatic void overwrite(output int b);
    b = 1;
    aliased = 2;
    b = b + aliased;
  endfunction

  function automatic void probe(output logic [7:0] v, output bit was_unknown);
    was_unknown = (v === 8'hxx);
    v = 8'hAA;
  endfunction

  initial begin
    scalar = 99;
    get_five(scalar);

    two(first, second);

    quotient = divide(10, remainder);

    aliased = 99;
    overwrite(aliased);

    observed = 8'h55;
    probe(observed, started_unknown);
  end

  final begin
    if (scalar !== 5) $fatal(1, "scalar was %0d, expected 5", scalar);
    if (first !== 3) $fatal(1, "first was %0d, expected 3", first);
    if (second !== 7) $fatal(1, "second was %0d, expected 7", second);
    if (quotient !== 3) $fatal(1, "quotient was %0d, expected 3", quotient);
    if (remainder !== 1) $fatal(1, "remainder was %0d, expected 1", remainder);
    if (aliased !== 3) $fatal(1, "aliased was %0d, expected 3", aliased);
    if (observed !== 8'hAA)
      $fatal(1, "observed was %h, expected aa", observed);
    if (started_unknown !== 1'b1)
      $fatal(1, "started_unknown was %b, expected 1", started_unknown);
    $display("All checks passed");
  end
endmodule
