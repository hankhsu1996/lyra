module Top;
  bit const_oob_2state;
  bit runtime_oob_2state;
  bit negative_idx_2state;
  initial begin
    bit [7:0] data;
    int idx;
    data = 8'b1111_1111;
    const_oob_2state = data[8];
    idx = 8; runtime_oob_2state = data[idx];
    idx = -1; negative_idx_2state = data[idx];
  end
endmodule
