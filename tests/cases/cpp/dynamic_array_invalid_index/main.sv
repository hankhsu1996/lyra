module Top;
  // LRM 7.4.5 + Table 7-1: an index that is out of bounds, or that carries any
  // x / z bit, is invalid. A read returns the element type's Table 7-1 default
  // (2-state integral -> 0, 4-state integral -> 'x); a write performs no
  // operation. A dynamic array obeys the same rule as a fixed unpacked array,
  // and a zero-size (unconstructed) array makes every index out of bounds.
  int int_arr [] = '{10, 20, 30};
  logic [7:0] logic_arr [] = '{8'h11, 8'h22, 8'h33};
  int empty_arr [];
  integer idx;

  int oob_2s_pos, oob_2s_neg, from_empty, in_range_2s;
  logic [7:0] oob_4s_pos, x_idx_4s, z_idx_4s, in_range_4s;

  initial begin
    // Reads: 2-state OOB -> 0, 4-state OOB -> 'x, X / Z index -> 'x, and a
    // read from a zero-size array is out of bounds at every index.
    idx = 100; oob_2s_pos  = int_arr[idx];
    idx = -5;  oob_2s_neg  = int_arr[idx];
    from_empty = empty_arr[0];
    idx = 1;   in_range_2s = int_arr[idx];
    idx = 100; oob_4s_pos  = logic_arr[idx];
    idx = 'x;  x_idx_4s    = logic_arr[idx];
    idx = 'z;  z_idx_4s    = logic_arr[idx];
    idx = 1;   in_range_4s = logic_arr[idx];

    // Writes under the same invalid-index conditions are no-ops; the originals
    // are unchanged.
    idx = 100; int_arr[idx]   = 999;
    idx = -5;  int_arr[idx]   = 888;
    idx = 'x;  logic_arr[idx] = 8'hAA;
    idx = 'z;  logic_arr[idx] = 8'hBB;
  end
endmodule
