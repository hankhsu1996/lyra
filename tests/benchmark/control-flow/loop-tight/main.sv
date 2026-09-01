// @measure: run
// @work: pass
//
// One pass runs a ten-thousand-iteration inner loop whose body is a single
// dependent XOR and shift, so what it costs is the loop's own back edge rather
// than anything in it. The dependency between iterations is what stops the
// whole loop folding to a constant.
module Top;
  localparam int INNER_SIZE = 10000;

  initial begin
    int num_passes;
    int acc;

    if (!$value$plusargs("work=%d", num_passes)) num_passes = 10;

    acc = 32'hCAFE_BABE;
    for (int pass = 0; pass < num_passes; pass++) begin
      for (int i = 0; i < INNER_SIZE; i++) begin
        acc = acc ^ (acc << 1) + i;
      end
    end

    $display("loop-tight done: acc=%0d", acc);
    $finish;
  end
endmodule
