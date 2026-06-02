module Top;
  logic [7:0] c;
  initial begin
    c = 8'bzzzz_zzzz;
    $display("%c", c);
  end
endmodule
