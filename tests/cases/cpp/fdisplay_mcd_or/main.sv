module Top;
  int mcd1, mcd2;
  initial begin
    mcd1 = $fopen("file1.txt");
    mcd2 = $fopen("file2.txt");
    // LRM 21.3.1: MCDs are bitwise OR-able to fan output to multiple sinks.
    // Bit 0 routes to stdout as well as both files.
    $fdisplay(1 | mcd1 | mcd2, "to all three");
    $fclose(mcd1);
    $fclose(mcd2);
  end
endmodule
