module Top;
  string sa [] = '{"banana", "apple", "cherry", "apple"};
  initial begin
    // No-with min uses String operator<; unique dedups by String value.
    $display("%p", sa.min);
    $display("%p", sa.unique);
    // with-key max compares by the closure key (still String here).
    $display("%p", sa.max with (item));
  end
endmodule
