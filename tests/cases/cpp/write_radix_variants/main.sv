module Top;
  initial begin
    // $write family: same as $display but no trailing newline. Three writes
    // on one line, then a $display to terminate the line.
    $write(8'hAB);
    $writeb(4'h5);
    $writeh(8'hAB);
    $writeo(8'h3F);
    $display("");
  end
endmodule
