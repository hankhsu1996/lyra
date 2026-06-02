module Top;
  logic [7:0] c;
  initial begin
    c = 8'bxxxx_xxxx;
    $display("%c", c);
  end
endmodule
