module Top;
  logic [7:0] b;

  initial begin
    bit [3:0] a;
    a = 4'b1010;
    b = a;
  end
endmodule
