module Top;
  logic [7:0] data;
  initial begin
    @(data[3]);
  end
endmodule
