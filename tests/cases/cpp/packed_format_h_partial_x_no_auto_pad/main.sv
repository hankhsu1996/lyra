module Top;
  logic [15:0] v;

  initial begin
    v = 16'b00000000_xxxx_xxxx;
    $display("%h", v);
  end
endmodule
