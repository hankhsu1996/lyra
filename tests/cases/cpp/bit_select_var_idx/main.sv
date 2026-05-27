module Top;
  bit at0;
  bit at1;
  bit at2;
  bit at3;
  initial begin
    bit [7:0] data;
    int idx;
    data = 8'b0000_1101;
    idx = 0; at0 = data[idx];
    idx = 1; at1 = data[idx];
    idx = 2; at2 = data[idx];
    idx = 3; at3 = data[idx];
  end
endmodule
