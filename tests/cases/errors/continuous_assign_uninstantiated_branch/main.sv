module Top;
  logic [3:0] q;
  logic [3:0] sel;

  // The else branch is uninstantiated for the loop's canonical iteration
  // (fb == 0), where its part-select bound `fb-1` goes out of range; Lyra does
  // not yet lower such genvar-conditional generate branches and reports it
  // gracefully rather than crashing.
  for (genvar fb = 0; fb < 4; fb++) begin : g
    if (fb == 0) begin : gz
      assign sel[fb] = q[fb];
    end else begin : gr
      assign sel[fb] = &q[fb-1:0];
    end
  end
endmodule
