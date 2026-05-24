module Top;
  initial begin
    int i = 0;
    int evens = 0;
    repeat (6) begin
      if (i < 3)
        evens = evens + 1;
      i = i + 1;
    end
    $display("evens=%0d i=%0d", evens, i);
  end
endmodule
