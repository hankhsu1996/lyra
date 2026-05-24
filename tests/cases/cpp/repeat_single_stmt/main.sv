module Top;
  initial begin
    int x = 0;
    repeat (4) x = x + 1;
    $display("x=%0d", x);
  end
endmodule
