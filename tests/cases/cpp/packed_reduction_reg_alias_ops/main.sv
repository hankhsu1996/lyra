module Top;
  reg and_with_z;

  initial begin
    reg [3:0] a;
    a = 4'b11z1;
    and_with_z = &a;
  end
endmodule
