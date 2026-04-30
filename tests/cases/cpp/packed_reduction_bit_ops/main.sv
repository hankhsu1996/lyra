module Top;
  bit [3:0] a;

  initial begin
    a = 4'b1111; $display("%b", &a);
    a = 4'b1011; $display("%b", &a);

    a = 4'b0000; $display("%b", |a);
    a = 4'b0100; $display("%b", |a);

    a = 4'b0000; $display("%b", ^a);
    a = 4'b0001; $display("%b", ^a);
    a = 4'b0011; $display("%b", ^a);

    a = 4'b1111; $display("%b", ~&a);
    a = 4'b1011; $display("%b", ~&a);

    a = 4'b0000; $display("%b", ~|a);
    a = 4'b0100; $display("%b", ~|a);

    a = 4'b0000; $display("%b", ~^a);
    a = 4'b0001; $display("%b", ~^a);
    a = 4'b0011; $display("%b", ~^a);
  end
endmodule
