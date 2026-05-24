module Top;
  int i;
  initial begin
    for (i = 0; i < 10; i = i + 1) begin
      if (i == 4) break;
    end
  end
endmodule
