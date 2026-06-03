module Top;
  // LRM 21.3.3 NOTE: $sformat / $sformatf accept a non-literal format
  // expression (string variable, integral, unpacked array of byte). This
  // build defers all three to a follow-up runtime format parser; each is
  // rejected at lowering with the same diagnostic shape.
  string  fmt_var;
  int     a;
  string  s;
  initial begin
    fmt_var = "%d";
    $sformat(s, fmt_var, a);
  end
endmodule
