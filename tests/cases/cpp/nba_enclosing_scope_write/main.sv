module Top;
  bit clk;
  int d;
  int q_if;
  int q_for [2];

  // Nonblocking write to an enclosing-scope variable from inside a conditional
  // generate scope: q_if lives on Top, the always_ff lives in g_if.
  if (1) begin : g_if
    always_ff @(posedge clk) q_if <= d;
  end

  // Nonblocking write to an enclosing-scope array element from inside a loop
  // generate scope: q_for lives on Top, indexed by the generate genvar.
  for (genvar i = 0; i < 2; i++) begin : g_for
    always_ff @(posedge clk) q_for[i] <= d + i;
  end

  initial begin
    clk = 0;
    d = 7;
    #5 clk = 1;
    #5 clk = 0;
    $finish;
  end
endmodule
