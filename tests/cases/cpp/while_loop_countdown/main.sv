module Top;
  int i;
  int product;
  initial begin
    i = 4;
    product = 1;
    while (i > 0) begin
      product = product * i;
      i = i - 1;
    end
  end
endmodule
