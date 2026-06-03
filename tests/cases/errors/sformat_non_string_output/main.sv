module Top;
  // LRM 21.3.3 allows integral and unpacked-array-of-byte outputs via
  // LRM 5.9 string-literal-assignment rules; Cut 1 defers those and only
  // accepts a string-typed output_var.
  int    out_int;
  int    a;
  initial begin
    a = 7;
    $sformat(out_int, "%d", a);
  end
endmodule
