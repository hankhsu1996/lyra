module Top;
  int hits;
  int i;
  initial begin
    hits = 0;
    i = 5;
    while (i < 0) begin
      hits = hits + 1;
      i = i + 1;
    end
  end
endmodule
