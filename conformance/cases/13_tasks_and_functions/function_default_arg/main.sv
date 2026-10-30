// A formal argument may carry a default value that the call takes when the
// argument is omitted or written as an empty placeholder. The default
// expression belongs to the scope containing the declaration and is evaluated
// once for each call that uses it, so a default naming a variable follows that
// variable's value at the time of the call (LRM 13.5.3).
module Top;
  int base;
  int omitted;
  int placeholder;
  int early;
  int late;

  function automatic int addab(int a, int b = 5);
    return a + b;
  endfunction

  function automatic int read3(int a = 1, int b = 2, int c = 3);
    return a * 100 + b * 10 + c;
  endfunction

  function automatic int addbase(int x, int y = base);
    return x + y;
  endfunction

  initial begin
    omitted = addab(7);
    placeholder = read3(, 5);
    base = 10;
    early = addbase(1);
    base = 100;
    late = addbase(1);
  end

  final begin
    if (omitted !== 12) $fatal(1, "omitted was %0d, expected 12", omitted);
    if (placeholder !== 153)
      $fatal(1, "placeholder was %0d, expected 153", placeholder);
    if (early !== 11) $fatal(1, "early was %0d, expected 11", early);
    if (late !== 101) $fatal(1, "late was %0d, expected 101", late);
    $display("All checks passed");
  end
endmodule
