module Top;
  logic [7:0] c;
  initial begin
    c = 8'h41;
    $display("%c", c);
    c = 8'h61;
    $display("%c", c);
    c = 8'h30;
    $display("%c", c);
  end
endmodule
