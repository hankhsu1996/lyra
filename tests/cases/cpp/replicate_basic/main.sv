module Top;
  byte x;
  byte signed_x;
  bit [3:0] nib;
  bit [31:0] four_bytes;
  bit [15:0] four_nibbles;
  bit [7:0] one_byte;
  bit [31:0] signed_replicated;
  bit [7:0] all_ones;

  initial begin
    x = 8'hAB;
    signed_x = -1;
    nib = 4'hF;
    four_bytes = {4{x}};
    four_nibbles = {4{nib}};
    one_byte = {1{x}};
    signed_replicated = {4{signed_x}};
    all_ones = {8{1'b1}};
  end
endmodule
