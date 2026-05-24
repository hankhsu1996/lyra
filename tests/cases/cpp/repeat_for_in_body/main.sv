module Top;
  initial begin
    int total = 0;
    repeat (3) begin
      for (int j = 0; j < 3; j = j + 1) begin
        total = total + (j + 1);
      end
    end
    $display("total=%0d", total);
  end
endmodule
