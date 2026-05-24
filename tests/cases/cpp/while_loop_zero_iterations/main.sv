module Top;
  initial begin
    int hits = 0;
    int i = 5;
    while (i < 0) begin
      hits = hits + 1;
      i = i + 1;
    end
    $display("hits=%0d i=%0d", hits, i);
  end
endmodule
