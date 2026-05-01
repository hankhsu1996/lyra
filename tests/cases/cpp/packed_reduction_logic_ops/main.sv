module Top;
  logic [3:0] a;
  logic y;

  initial begin
    a = 4'b1111; y = &a;  $display("%b", y);
    a = 4'b11x1; y = &a;  $display("%b", y);
    a = 4'b10x1; y = &a;  $display("%b", y);

    a = 4'b0000; y = |a;  $display("%b", y);
    a = 4'b00z0; y = |a;  $display("%b", y);
    a = 4'b01z0; y = |a;  $display("%b", y);

    a = 4'b1011; y = ^a;  $display("%b", y);
    a = 4'b10x1; y = ^a;  $display("%b", y);

    a = 4'b1111; y = ~&a; $display("%b", y);
    a = 4'b0000; y = ~|a; $display("%b", y);
    a = 4'b1011; y = ~^a; $display("%b", y);
    a = 4'b10z1; y = ~^a; $display("%b", y);
  end
endmodule
