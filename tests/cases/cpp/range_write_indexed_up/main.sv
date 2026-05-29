module Top;
  bit [31:0] out0;
  bit [31:0] out8;
  bit [31:0] out_var;
  initial begin
    int idx;
    out0 = 32'h00000000;
    out0[0+:8] = 8'hFF;
    out8 = 32'h00000000;
    out8[8+:8] = 8'hFF;
    idx = 20;
    out_var = 32'h00000000;
    out_var[idx+:8] = 8'hFF;
  end
endmodule
