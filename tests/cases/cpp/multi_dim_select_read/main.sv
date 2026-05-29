module Top;
  bit [7:0] elem0;
  bit [7:0] elem1;
  bit [3:0] high_nibble_1;
  bit bit_in_elem;
  initial begin
    bit [1:0][7:0] data;
    int idx;
    data[0] = 8'hAB;
    data[1] = 8'hCD;
    elem0 = data[0];
    elem1 = data[1];
    idx = 4;
    high_nibble_1 = data[1][idx+:4];
    bit_in_elem = data[1][0];
  end
endmodule
