module Top;
  logic [7:0] data;
  int   seen;
  initial begin
    data = 8'h00;
    seen = 0;
    @(data);
    seen = 1;
  end
  initial begin
    #5;
    data = 8'h42;
  end
endmodule
