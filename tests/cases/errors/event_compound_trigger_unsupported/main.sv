module Top;
  logic [7:0] bus = 0;
  initial begin
    @(bus[3] | bus[5]);
  end
endmodule
