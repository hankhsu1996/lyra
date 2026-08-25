module Top;
  logic [7:0] byte_value = 8'hA5;
  int ones;

  initial begin
    ones = $countones(byte_value);
    $display("%0d", ones);
  end
endmodule
