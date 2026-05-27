module Top;
  logic [127:0] add_xz;
  logic [127:0] sub_xz;
  logic [127:0] mul_xz;
  logic [127:0] neg_xz;
  logic [127:0] shl_xz;
  logic [127:0] shr_xz_amt;
  logic cmp_xz;
  logic [127:0] shl_xz_partial;

  initial begin
    logic [127:0] a_known;
    logic [127:0] a_xz;
    logic [127:0] partial;

    a_known = 128'd100;
    a_xz = 128'bxxxx;

    add_xz = a_known + a_xz;
    sub_xz = a_known - a_xz;
    mul_xz = a_known * a_xz;
    neg_xz = -a_xz;
    shl_xz = a_xz << 4;
    shr_xz_amt = a_known >> a_xz;
    cmp_xz = (a_known < a_xz);

    partial = 128'hff;
    shl_xz_partial = partial << 60;
  end
endmodule
