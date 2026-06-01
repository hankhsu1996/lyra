// `edge` keyword fires on both posedge and negedge.
module Top;
  logic [7:0] bus = 8'h00;
  int count = 0;
  initial begin
    @(edge bus[3]);
    count = count + 1;
    @(edge bus[3]);
    count = count + 1;
  end
  initial begin
    #5 bus = 8'b0000_1000;
    #5 bus = 8'h00;
  end
endmodule
