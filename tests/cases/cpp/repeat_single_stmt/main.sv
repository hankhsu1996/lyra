module Top;
  int x;
  initial begin
    x = 0;
    repeat (4) x = x + 1;
  end
endmodule
