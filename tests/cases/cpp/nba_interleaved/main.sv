module Top;
  int q;
  initial begin
    q = 1;
    q <= 2;
    q = 3;
    #1;
  end
endmodule
