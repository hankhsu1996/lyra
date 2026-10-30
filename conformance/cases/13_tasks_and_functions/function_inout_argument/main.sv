// An inout argument is copied in at the call and copied back out at the
// return, so the body reads the actual's value on entry, the actual still
// carries that value while the body runs, and it takes the formal's value only
// on exit. Input, output, and inout formals may be given in one argument list,
// each direction keeping its own behaviour (LRM 13.4, 13.5).
module Top;
  int counted;
  int produced;
  int accumulated;
  int seen_on_entry;
  int seen_after_write;

  function automatic void inc(inout int v);
    v = v + 1;
    seen_after_write = counted;
  endfunction

  function automatic void f(input int a, output int b, inout int c);
    seen_on_entry = c;
    b = a * 2;
    c = c + a;
  endfunction

  initial begin
    counted = 10;
    inc(counted);

    produced = 0;
    accumulated = 100;
    f(5, produced, accumulated);
  end

  final begin
    if (counted !== 11) $fatal(1, "counted was %0d, expected 11", counted);
    if (seen_after_write !== 10)
      $fatal(1, "seen_after_write was %0d, expected 10", seen_after_write);
    if (produced !== 10) $fatal(1, "produced was %0d, expected 10", produced);
    if (accumulated !== 105)
      $fatal(1, "accumulated was %0d, expected 105", accumulated);
    if (seen_on_entry !== 100)
      $fatal(1, "seen_on_entry was %0d, expected 100", seen_on_entry);
    $display("All checks passed");
  end
endmodule
