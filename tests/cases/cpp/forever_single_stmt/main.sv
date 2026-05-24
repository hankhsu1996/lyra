module Top;
  int i;
  initial begin
    i = 0;
    forever if (i == 3) break; else i = i + 1;
  end
endmodule
