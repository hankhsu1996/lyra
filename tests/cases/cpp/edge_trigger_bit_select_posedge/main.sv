module Top;
  logic [7:0] bus = 8'h00;
  int result = 0;
  initial begin
    @(posedge bus[3]);
    result = 1;
  end
  initial #5 bus = 8'b0000_1000;
endmodule
