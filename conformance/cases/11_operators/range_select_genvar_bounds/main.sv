// Within each elaborated instance of a loop generate block the loop index is
// an integer parameter fixed at the value the instance was elaborated with,
// so it is a constant expression and may appear in part-select bounds. Every
// instance therefore gets its own select, and instances may differ in the
// width the select yields (LRM 11.5.1, 27.4).
module Top;
  logic [3:0] source;
  logic [31:0] lanes;
  logic [3:0] parity;

  assign source = 4'b1011;

  for (genvar i = 0; i < 2; i++) begin : g_lane
    assign lanes[16*i+15 : 16*i] = 16'hABCD + i;
  end

  for (genvar i = 0; i < 4; i++) begin : g_parity
    if (i > 0) begin : g_prefix
      assign parity[i] = ^source[i-1:0];
    end else begin : g_first
      assign parity[i] = source[0];
    end
  end

  final begin
    if (lanes !== 32'hABCEABCD)
      $fatal(1, "lanes was %h, expected abceabcd", lanes);
    if (parity !== 4'b0011)
      $fatal(1, "parity was %b, expected 0011", parity);
    $display("All checks passed");
  end
endmodule
