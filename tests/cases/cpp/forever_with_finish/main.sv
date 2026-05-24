module Top;
  int counter;
  initial begin
    counter = 0;
    forever begin
      counter = counter + 1;
      if (counter >= 5) $finish;
    end
  end
endmodule
