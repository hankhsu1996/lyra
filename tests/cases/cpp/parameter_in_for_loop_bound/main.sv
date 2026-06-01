module Top;
  parameter int LIMIT = 4;
  int total = 0;
  initial begin
    for (int i = 0; i < LIMIT; i = i + 1) begin
      total = total + i;
    end
  end
endmodule
