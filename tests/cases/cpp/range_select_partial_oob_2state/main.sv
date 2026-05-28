module Top;
  bit [3:0] partial_high;
  bit [3:0] partial_low;
  initial begin
    bit [7:0] data;
    int idx;
    data = 8'b1111_1111;
    idx = 6;  partial_high = data[idx +: 4];
    idx = -2; partial_low  = data[idx +: 4];
  end
endmodule
