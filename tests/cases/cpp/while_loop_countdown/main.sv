module Top;
  initial begin
    int i = 4;
    int product = 1;
    while (i > 0) begin
      product = product * i;
      i = i - 1;
    end
    $display("i=%0d product=%0d", i, product);
  end
endmodule
