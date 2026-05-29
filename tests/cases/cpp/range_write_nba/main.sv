module Top;
  bit [31:0] final_data;
  initial begin
    int idx;
    final_data = 32'h0000ABCD;
    idx = 8;
    final_data[idx+:8] <= 8'hEF;
    #1;
  end
endmodule
