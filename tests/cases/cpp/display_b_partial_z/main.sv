module Top;
  logic [3:0] v;
  initial begin
    v = 4'b0z1z;
    $display("%b", v);
  end
endmodule
