module Top;
  initial begin
    for (int i = 0; i < 15; i = i + 1) begin
      $warning("repeating");
    end
  end
endmodule
