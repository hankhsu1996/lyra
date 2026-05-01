module Top;
  bit [3:0] a;
  bit y;

  initial begin
    a = 4'b1111; y = &a;  $display("%b", y);
    a = 4'b1101; y = &a;  $display("%b", y);
    a = 4'b0000; y = |a;  $display("%b", y);
    a = 4'b0100; y = |a;  $display("%b", y);
    a = 4'b1011; y = ^a;  $display("%b", y);
    a = 4'b1010; y = ^a;  $display("%b", y);
    a = 4'b1111; y = ~&a; $display("%b", y);
    a = 4'b0000; y = ~|a; $display("%b", y);
    a = 4'b1011; y = ~^a; $display("%b", y);
  end
endmodule
