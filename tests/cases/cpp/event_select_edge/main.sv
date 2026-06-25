// LRM 9.4.2 edge events on a multi-bit expression: the edge is detected on the
// LSB of the selected bits only, and a change outside the projection must not
// wake. Each waiter isolates one form:
//   posedge b_whole on a whole vector (LSB is bit 0; an upper-bits-only write
//   must not wake), posedge b_nft[3] with bit-5 noise (nft_snap proves the t=5
//   noise write does not wake), negedge b_neg[3], `edge` b_both[3] (count 2),
//   posedge b_rng[7:4] (LSB is bit 4; bit-7 noise must not wake), and indexed
//   part-selects b_up[3 +: 4] / b_dn[6 -: 4] (both span bits 6:3, LSB bit 3).
module Top;
  logic [7:0] b_whole = 8'h00;
  logic [7:0] b_nft = 8'h00;
  logic [7:0] b_neg = 8'b0000_1000;
  logic [7:0] b_both = 8'h00;
  logic [7:0] b_rng = 8'h00;
  logic [7:0] b_up = 8'h00;
  logic [7:0] b_dn = 8'h00;

  int whole = 0;
  int whole_snap = 99;
  int nft = 0;
  int nft_snap = 99;
  int neg = 0;
  int both = 0;
  int rng = 0;
  int rng_snap = 99;
  int up = 0;
  int dn = 0;

  initial begin
    @(posedge b_whole);
    whole = 1;
  end
  initial begin
    @(posedge b_nft[3]);
    nft = 1;
  end
  initial begin
    @(negedge b_neg[3]);
    neg = 1;
  end
  initial begin
    @(edge b_both[3]);
    both = both + 1;
    @(edge b_both[3]);
    both = both + 1;
  end
  initial begin
    @(posedge b_rng[7:4]);
    rng = 1;
  end
  initial begin
    @(posedge b_up[3 +: 4]);
    up = 1;
  end
  initial begin
    @(posedge b_dn[6 -: 4]);
    dn = 1;
  end

  initial begin
    #5;
    b_whole = 8'hF0;
    b_nft = 8'b0010_0000;
    b_rng = 8'b1000_0000;
    whole_snap = whole;
    nft_snap = nft;
    rng_snap = rng;
    #5;
    b_whole = 8'hF1;
    b_neg[3] = 0;
    b_both[3] = 1;
    b_nft[3] = 1;
    b_rng[4] = 1;
    b_up[3] = 1;
    b_dn[3] = 1;
    #5;
    b_both[3] = 0;
  end
endmodule
