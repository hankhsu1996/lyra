module Top;
  int q;
  initial begin
    q = 5;
    q <= q + 10;
    q = 100;
    #1;
  end
endmodule
