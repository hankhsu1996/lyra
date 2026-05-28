module Top;
  int a, b, sum, diff;

  initial begin
    a = 1;
    b = 0;
    #1;
    a = 7;
    b = 2;
    #1;
  end

  always @* begin
    sum  = a + b;
    diff = a - b;
  end
endmodule
