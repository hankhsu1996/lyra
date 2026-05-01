module Top;
  bit [64:0] a;

  initial begin
    a = 65'h1_ffff_ffff_ffff_ffff;
    $display("%h", a);
  end
endmodule
