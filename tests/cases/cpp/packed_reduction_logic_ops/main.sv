module Top;
  logic [3:0] a;

  initial begin
    a = 4'b1111; $display("%b", &a);
    a = 4'b11x1; $display("%b", &a);
    a = 4'b10x1; $display("%b", &a);

    a = 4'b0000; $display("%b", |a);
    a = 4'b00x0; $display("%b", |a);
    a = 4'b01x0; $display("%b", |a);

    a = 4'b0011; $display("%b", ^a);
    a = 4'b0001; $display("%b", ^a);
    a = 4'b00x1; $display("%b", ^a);

    a = 4'b1111; $display("%b", ~&a);
    a = 4'b11x1; $display("%b", ~&a);
    a = 4'b10x1; $display("%b", ~&a);

    a = 4'b0000; $display("%b", ~|a);
    a = 4'b00x0; $display("%b", ~|a);
    a = 4'b01x0; $display("%b", ~|a);

    a = 4'b0011; $display("%b", ~^a);
    a = 4'b0001; $display("%b", ~^a);
    a = 4'b00x1; $display("%b", ~^a);

    a = 4'b11z1; $display("%b", &a);
    a = 4'b00z0; $display("%b", |a);
    a = 4'b10z1; $display("%b", ~&a);
  end
endmodule
