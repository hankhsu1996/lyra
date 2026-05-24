module Top;
  int x;
  initial begin
    x = 1;
    $finish;
    x = 2;
  end
endmodule
