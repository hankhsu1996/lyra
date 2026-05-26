module Top;
  logic [7:0] b_from_x;
  logic [7:0] b_from_z;

  initial begin
    logic signed [3:0] a;
    a = 4'bx010;
    b_from_x = a;
    a = 4'bz010;
    b_from_z = a;
  end
endmodule
