module Top;
  logic [7:0] l;
  initial begin
    l = 8'b10xz0101;
    $display("%b", l);
  end
endmodule
