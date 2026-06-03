module Top;
  // Procedural locals are ByValueCapture'd at submit time, not re-read at
  // postponed-fire time. Without the snapshot the closure body would either
  // observe the post-mutation value (99) or dangle into a destroyed initial
  // frame; with the snapshot it prints the value as of the $strobe call (5).
  initial begin
    int x;
    x = 5;
    $strobe("%0d", x);
    x = 99;
  end
endmodule
