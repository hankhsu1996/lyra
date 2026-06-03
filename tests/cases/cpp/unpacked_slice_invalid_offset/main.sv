module Top;
  int int_arr   [6] = '{10, 20, 30, 40, 50, 60};
  int int_src   [3] = '{77, 88, 99};
  logic [7:0] logic_arr [6] = '{8'h10, 8'h20, 8'h30, 8'h40, 8'h50, 8'h60};
  logic [7:0] logic_src [3] = '{8'hAA, 8'hBB, 8'hCC};

  integer idx;
  integer x_idx;

  int int_dst [3];
  logic [7:0] logic_dst [3];
  logic [7:0] x_dst [3];

  initial begin
    // Partial-OOB read: positions [4, 5, 6] on a size-6 source; pos 6 OOB.
    // 2-state element default -> 0; 4-state element default -> 'x.
    idx = 4;
    int_dst = int_arr[idx +: 3];
    logic_dst = logic_arr[idx +: 3];

    // X-bit offset on read -> whole slice defaults to Table 7-1.
    x_idx = 'x;
    x_dst = logic_arr[x_idx +: 3];

    // Partial-OOB write: in-range portion writes, OOB portion drops.
    idx = 4;
    int_arr[idx +: 3] = int_src;

    // X-bit offset on write -> no-op; original array unchanged.
    logic_arr[x_idx +: 3] = logic_src;
  end
endmodule
