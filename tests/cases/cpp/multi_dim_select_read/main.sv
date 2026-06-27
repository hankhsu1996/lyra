module Top;
  bit [7:0] elem0;
  bit [7:0] elem1;
  bit [3:0] high_nibble_1;
  bit bit_in_elem;
  bit [31:0] full3d;
  bit [7:0] elem3d;
  bit [3:0] nib3d;
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

    // A chained read on a 3-D packed array: `d3[1]` is itself a 2-D vector, so
    // the inner select must see its dimensions rather than a flattened bit run.
    begin
      bit [1:0][1:0][7:0] d3;
      d3[0][0] = 8'h11;
      d3[0][1] = 8'h22;
      d3[1][0] = 8'h33;
      d3[1][1] = 8'h44;
      full3d = d3;
      elem3d = d3[1][0];
      nib3d = d3[1][0][3:0];
    end
  end
endmodule
