module Top;
  int i;
  int evens;
  initial begin
    i = 0;
    evens = 0;
    forever begin
      i = i + 1;
      if (i > 6) break;
      if ((i % 2) == 1) continue;
      evens = evens + i;
    end
  end
endmodule
