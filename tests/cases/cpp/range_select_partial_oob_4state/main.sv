module Top;
  logic [3:0] partial_high;
  logic [3:0] partial_low;
  initial begin
    logic [7:0] data;
    integer idx;
    data = 8'b1111_1111;
    idx = 6;  partial_high = data[idx +: 4];
    idx = -2; partial_low  = data[idx +: 4];
  end
endmodule
