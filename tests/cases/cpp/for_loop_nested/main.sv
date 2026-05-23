module Top;
  initial begin
    int total = 0;
    for (int i = 0; i < 3; i = i + 1)
      for (int j = 0; j < 3; j = j + 1)
        total = total + 1;
    $display("total=%0d", total);
  end
endmodule
