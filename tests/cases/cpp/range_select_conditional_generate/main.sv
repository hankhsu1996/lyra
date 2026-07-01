module Top;
  // A variable-width packed range-select inside a conditional generate arm whose
  // width depends on the loop genvar: `in[i-1:0]` is `logic[0:0]` at i=1,
  // `logic[1:0]` at i=2, `logic[2:0]` at i=3. Each iteration is elaborated at its
  // own concrete genvar value, so each has its own valid, concretely-typed body;
  // the `else` arm is the one active at i=0, the `then` arm at i>0.
  logic [3:0] in;
  logic [3:0] out;
  assign in = 4'b1011;
  for (genvar i = 0; i < 4; i++) begin : g
    if (i > 0) begin : c
      assign out[i] = ^in[i-1:0];
    end
    else begin : b
      assign out[i] = in[0];
    end
  end
endmodule
