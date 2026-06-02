module Top;
  initial begin
    // LRM 21.3.1: MCD bit 0 always refers to stdout. Passing the literal 1
    // routes the output through the same stdout dispatch as $display so the
    // test harness's stdout matcher sees it.
    $fdisplay(1, "hello from fdisplay");
    $fwrite(1, "no newline");
    $display("");
  end
endmodule
