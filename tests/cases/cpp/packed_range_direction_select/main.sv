module Top;
  bit [0:7] asc_bit_w;
  bit asc_bit_r;
  bit [-1:6] neg_bit_w;
  bit [0:7] asc_slice_w;
  bit [6:1] desc_nonzero_w;
  bit [7:0] asc_pselect_up;
  bit [7:0] asc_pselect_dn;
  bit [6:1] dn_src;
  bit fs_idx_r;
  initial begin
    bit [0:7] src;
    integer fi;
    asc_bit_w = 8'h00;
    asc_bit_w[2] = 1'b1;
    src = 8'b00100000;
    asc_bit_r = src[2];
    neg_bit_w = 8'h00;
    neg_bit_w[0] = 1'b1;
    asc_slice_w = 8'h00;
    asc_slice_w[3:5] = 3'b111;
    desc_nonzero_w = 6'h00;
    desc_nonzero_w[3] = 1'b1;
    src = 8'b10110010;
    asc_pselect_up = src[1 +: 3];
    asc_pselect_dn = src[5 -: 3];
    // A four-state index over a non-zero-based range: the LSB-rebase subtracts
    // the declared `right` (a two-state literal) from the `integer` index, so
    // the two operands' domains must reconcile.
    dn_src = 6'b010100;
    fi = 3;
    fs_idx_r = dn_src[fi];
  end
endmodule
