module Top;
  logic [7:0] b;

  initial begin
    logic [3:0] a;
    a = 4'b10xz;
    b = a;
  end
endmodule
