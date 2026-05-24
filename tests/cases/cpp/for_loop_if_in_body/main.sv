module Top;
  int nonzero;
  int zero;
  initial begin
    nonzero = 0;
    zero = 0;
    for (int i = 0; i < 6; i = i + 1) begin
      int step = 1;
      if (i)
        nonzero = nonzero + step;
      else
        zero = zero + step;
    end
  end
endmodule
