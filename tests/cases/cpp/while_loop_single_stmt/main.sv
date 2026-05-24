module Top;
  initial begin
    int i = 0;
    while (i < 7) i = i + 1;
    $display("i=%0d", i);
  end
endmodule
