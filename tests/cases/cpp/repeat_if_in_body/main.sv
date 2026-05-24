module Top;
  int i;
  int evens;
  initial begin
    i = 0;
    evens = 0;
    repeat (6) begin
      if (i < 3)
        evens = evens + 1;
      i = i + 1;
    end
  end
endmodule
