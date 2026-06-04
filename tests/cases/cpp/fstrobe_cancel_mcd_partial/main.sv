module Top;
  int mcd_a, mcd_b;
  initial begin
    mcd_a = $fopen("a.txt");
    mcd_b = $fopen("b.txt");
    $fstrobe(mcd_a | mcd_b, "fans out");
    // LRM 21.3.2: closing any participating channel cancels the whole
    // postponed operation; mcd_b is still open at fire time but b.txt
    // is still empty.
    $fclose(mcd_a);
    #1 $fclose(mcd_b);
  end
endmodule
