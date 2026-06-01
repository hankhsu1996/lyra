module Top;
  logic [7:0] bus = 0;
  int i = 3;
  initial begin
    @(posedge bus[i]);
  end
endmodule
