module Top;
  int i;
  initial begin
    i = 0;
    repeat (10) begin
      if (i == 3) break;
      i = i + 1;
    end
  end
endmodule
