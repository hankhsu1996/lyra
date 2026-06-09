module Top;
  initial begin
    int total = 0;
    for (int i = 0; i < 3; i = i + 1) begin
      automatic int k = 0;
      do begin
        total = total + 1;
        k = k + 1;
      end while (k < 4);
    end
    $display("total=%0d", total);
  end
endmodule
