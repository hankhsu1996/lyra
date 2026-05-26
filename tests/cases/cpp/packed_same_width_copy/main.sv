module Top;
  bit [3:0] b_copy;
  logic [3:0] y_copy;

  initial begin
    bit [3:0] a;
    logic [3:0] x;
    a = 4'b1100;
    b_copy = a;
    x = 4'b10xz;
    y_copy = x;
  end
endmodule
