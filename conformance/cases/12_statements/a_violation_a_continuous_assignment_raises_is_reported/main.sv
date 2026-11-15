// @reports: violation
//
// A violation report waits for the Observed region and is discarded only at a
// flush point: a procedure resuming from an event control or a wait
// statement, or an always_comb or always_latch resumed by a transition on what
// it reads (LRM 12.4.2.1). A continuous assignment is a process of neither kind
// (LRM 4.9.1), so the violation one of its evaluations raises through a
// function it calls is reported, even though the evaluation that follows in the
// same time step matches an item.
module Top;
  function automatic int decode(logic [1:0] v);
    unique case (v)
      2'b01: return 1;
      2'b10: return 2;
    endcase
    return 0;
  endfunction

  logic [1:0] code = 2'b01;
  int decoded;
  assign decoded = decode(code);

  initial begin
    #1;
    if (decoded !== 1) $fatal(1, "decoded was %0d, expected 1", decoded);
    $display("Ready for the report");
    code = 2'b00;
    #0 code = 2'b01;
    #1;
    if (decoded !== 1) $fatal(1, "decoded was %0d, expected 1", decoded);
    $display("All checks passed");
  end
endmodule
