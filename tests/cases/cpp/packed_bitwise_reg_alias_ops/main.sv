module Top;
  reg [3:0] and_;

  initial begin
    reg [3:0] a;
    reg [3:0] b;
    a = 4'bz0x1;
    b = 4'b1011;
    and_ = a & b;
  end
endmodule
