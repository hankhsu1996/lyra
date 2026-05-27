module Top;
  logic const_oob_4state;
  logic x_index_4state;
  logic z_index_4state;
  logic inrange_known_idx;
  initial begin
    logic [7:0] data;
    integer idx;
    data = 8'b1111_1111;
    const_oob_4state = data[8];
    idx = 'x; x_index_4state = data[idx];
    idx = 'z; z_index_4state = data[idx];
    idx = 3; inrange_known_idx = data[idx];
  end
endmodule
