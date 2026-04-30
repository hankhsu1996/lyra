module Top;
  reg [3:0] a;

  initial begin
    a = 4'b11x1; $display("%b", &a);
    a = 4'b00x0; $display("%b", |a);
    a = 4'b00x1; $display("%b", ^a);
    a = 4'b10x1; $display("%b", ~&a);
    a = 4'b01x0; $display("%b", ~|a);
    a = 4'b0001; $display("%b", ~^a);
  end
endmodule
