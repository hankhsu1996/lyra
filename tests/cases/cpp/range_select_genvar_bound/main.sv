module Top;
  // Packed simple-range `[msb:lsb]` whose endpoints are genvar arithmetic (the
  // ibex butterfly form). Each generate instance writes a distinct 16-bit lane,
  // so all 32 bits are driven and the result is fully defined. The bounds stay
  // runtime expressions reading the constructor's induction variable -- they are
  // not folded to a single elaboration-time constant.
  logic [31:0] packed_out;
  for (genvar i = 0; i < 2; i++) begin : gen_lane
    assign packed_out[16*i + 15 : 16*i] = 16'hABCD;
  end
endmodule
