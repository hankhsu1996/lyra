module Top;
  int mcd;
  int is_single_bit;
  int bit31_clear;
  initial begin
    mcd = $fopen("mcd_check.txt");
    // LRM 21.3.1: MCD is a 32-bit value with exactly one bit set in 1..30
    // and bit 31 always clear.
    is_single_bit = ((mcd & (mcd - 1)) == 0 && mcd != 0) ? 1 : 0;
    bit31_clear = (mcd >= 0) ? 1 : 0;
    $fclose(mcd);
  end
endmodule
