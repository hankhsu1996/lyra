module Top;
  bit [15:0] a;
  initial begin
    a = 16'h1234;
    a[7:0]++;
    ++a[15:8];
  end
endmodule
