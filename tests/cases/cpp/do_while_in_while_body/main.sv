module Top;
  initial begin
    int outer = 0;
    int total = 0;
    while (outer < 3) begin
      int k = 0;
      do begin
        total = total + 1;
        k = k + 1;
      end while (k < 3);
      outer = outer + 1;
    end
    $display("outer=%0d total=%0d", outer, total);
  end
endmodule
