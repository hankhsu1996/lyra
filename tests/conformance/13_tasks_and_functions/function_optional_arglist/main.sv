// The empty parentheses of a call may be dropped when a void function takes no
// arguments, and equally when every argument it takes has a default value
// (LRM 13.5.5).
module Top;
  int plain;
  int defaulted;

  function automatic void set_plain;
    plain = 42;
  endfunction

  function automatic void set_defaulted(int v = 7);
    defaulted = v;
  endfunction

  initial begin
    set_plain;
    set_defaulted;
  end

  final begin
    if (plain !== 42) $fatal(1, "plain was %0d, expected 42", plain);
    if (defaulted !== 7) $fatal(1, "defaulted was %0d, expected 7", defaulted);
    $display("All checks passed");
  end
endmodule
