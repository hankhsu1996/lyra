module Top;
  logic [7:0] data;
  logic [7:0] one_bit;
  initial begin
    logic [3:0] idx;
    data = 8'hFF;
    idx = 4'bxxxx;
    data[idx+:4] = 4'b0000;
    one_bit = 8'hFF;
    one_bit[idx] = 1'b0;
  end
endmodule
