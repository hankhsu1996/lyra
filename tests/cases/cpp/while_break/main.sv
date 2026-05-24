module Top;
  int i;
  initial begin
    i = 0;
    while (1) begin
      if (i == 3) break;
      i = i + 1;
    end
  end
endmodule
