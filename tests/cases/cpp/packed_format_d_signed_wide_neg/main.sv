module Top;
  bit signed [127:0] a;

  initial begin
    a = 128'shffff_ffff_ffff_ffff_54ab_5673_14e0_f52e;
    $display("%d", a);
  end
endmodule
