module Top;
  int counter;
  initial begin
    counter = 0;
    while (1) begin
      counter = counter + 1;
      if (counter >= 5) $finish(0);
    end
  end
endmodule
