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

  int r2s_e0, r2s_e1, r2s_e2;
  logic [7:0] r4s_e0, r4s_e1, r4s_e2;
  logic [7:0] xr_e0, xr_e1, xr_e2;
  int w2s_e0, w2s_e1, w2s_e2, w2s_e3, w2s_e4, w2s_e5;
  logic [7:0] xw_e0, xw_e1, xw_e2, xw_e3, xw_e4, xw_e5;

  initial begin
    // Partial-OOB read: positions [4, 5, 6] on a size-6 source; pos 6 OOB.
    // 2-state element default -> 0; 4-state element default -> 'x.
    idx = 4;
    int_dst = int_arr[idx +: 3];
    r2s_e0 = int_dst[0]; r2s_e1 = int_dst[1]; r2s_e2 = int_dst[2];
    logic_dst = logic_arr[idx +: 3];
    r4s_e0 = logic_dst[0]; r4s_e1 = logic_dst[1]; r4s_e2 = logic_dst[2];

    // X-bit offset on read -> whole slice defaults to Table 7-1.
    x_idx = 'x;
    x_dst = logic_arr[x_idx +: 3];
    xr_e0 = x_dst[0]; xr_e1 = x_dst[1]; xr_e2 = x_dst[2];

    // Partial-OOB write: in-range portion writes, OOB portion drops.
    idx = 4;
    int_arr[idx +: 3] = int_src;
    w2s_e0 = int_arr[0]; w2s_e1 = int_arr[1]; w2s_e2 = int_arr[2];
    w2s_e3 = int_arr[3]; w2s_e4 = int_arr[4]; w2s_e5 = int_arr[5];

    // X-bit offset on write -> no-op; original array unchanged.
    logic_arr[x_idx +: 3] = logic_src;
    xw_e0 = logic_arr[0]; xw_e1 = logic_arr[1]; xw_e2 = logic_arr[2];
    xw_e3 = logic_arr[3]; xw_e4 = logic_arr[4]; xw_e5 = logic_arr[5];
  end
endmodule
