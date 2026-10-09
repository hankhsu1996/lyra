// A constant nothing reads, beside a loop of empty blocks. N is the constant's
// width in bits, which no block's meaning depends on.
module Top #(parameter int N = 4);
  localparam logic [N-1:0] P = '0;
  for (genvar i = 0; i < 64; i += 1) begin : g
  end
endmodule
