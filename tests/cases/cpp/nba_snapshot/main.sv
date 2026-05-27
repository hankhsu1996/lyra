module Top;
  int q;
  int t;
  initial begin
    q = 5;
    t = q;
    q = 10;
    q <= t + 1;
    q = 100;
    #1;
  end
endmodule
