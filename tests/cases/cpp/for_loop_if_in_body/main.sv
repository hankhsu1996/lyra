module Top;
  initial begin
    int nonzero = 0;
    int zero = 0;
    for (int i = 0; i < 6; i = i + 1) begin
      int step = 1;
      if (i)
        nonzero = nonzero + step;
      else
        zero = zero + step;
    end
    $display("nonzero=%0d", nonzero);
    $display("zero=%0d", zero);
  end
endmodule
