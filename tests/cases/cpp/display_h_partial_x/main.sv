module Top;
  logic [7:0] v;
  initial begin
    v = 8'b0011_xxxx;
    $display("%h", v);
  end
endmodule
