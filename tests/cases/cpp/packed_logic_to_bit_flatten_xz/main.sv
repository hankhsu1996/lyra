module Top;
  logic [3:0] a;
  bit [3:0] b;

  initial begin
    a = 4'b10xz;
    b = a;
    $display("%b", b);
  end
endmodule
