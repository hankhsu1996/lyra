module Top;
  int mcd1, mcd2;
  int both_valid;
  int reused_slot;
  initial begin
    mcd1 = $fopen("first.txt");
    $fclose(mcd1);
    // LRM 21.3.1: $fopen shall reuse channels that have been closed.
    mcd2 = $fopen("second.txt");
    both_valid = (mcd1 > 0 && mcd2 > 0) ? 1 : 0;
    reused_slot = (mcd1 == mcd2) ? 1 : 0;
    $fclose(mcd2);
  end
endmodule
