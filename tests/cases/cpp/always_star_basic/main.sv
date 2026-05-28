module Top;
  int a, b, c;

  initial begin
    a = 1;
    b = 2;
    #1;
    a = 5;
  end

  always @* c = a + b;
endmodule
