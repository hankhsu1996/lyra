module Top;
  logic [7:0] v;

  initial begin
    v = 8'b10x0z011;
    $display("%b", v);
  end
endmodule
