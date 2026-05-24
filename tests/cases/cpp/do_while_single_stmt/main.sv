module Top;
  initial begin
    int x = 0;
    do x = x + 1; while (x < 4);
    $display("x=%0d", x);
  end
endmodule
