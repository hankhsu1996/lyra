module Top;
  bit [11:0] a;

  initial begin
    a = 12'o0042;
    $display("%0o", a);
  end
endmodule
