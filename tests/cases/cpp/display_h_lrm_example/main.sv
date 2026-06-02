module Top;
  logic [13:0] v;
  initial begin
    v = 14'bx01010;
    $display("%h", v);
  end
endmodule
