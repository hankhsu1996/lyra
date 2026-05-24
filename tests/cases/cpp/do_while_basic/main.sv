module Top;
  initial begin
    int counter = 0;
    int sum = 0;
    do begin
      sum = sum + counter;
      counter = counter + 1;
    end while (counter < 5);
    $display("counter=%0d sum=%0d", counter, sum);
  end
endmodule
