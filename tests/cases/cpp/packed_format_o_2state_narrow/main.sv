module Top;
  bit [5:0] a;

  initial begin
    a = 6'o42;
    $display("%o", a);
  end
endmodule
