module Top;
  logic [7:0] partial_high;
  logic [7:0] partial_low;
  logic [7:0] fully_oob;
  initial begin
    int idx;
    partial_high = 8'hFF;
    idx = 6;
    partial_high[idx+:4] = 4'b0000;
    partial_low = 8'hFF;
    idx = -2;
    partial_low[idx+:4] = 4'b0000;
    fully_oob = 8'hFF;
    idx = 100;
    fully_oob[idx+:4] = 4'b0000;
  end
endmodule
