// $finish ends the simulation from whichever process reaches it (LRM 20.2),
// and a final procedure runs at that end (LRM 9.2.3). The process reaching it
// here is an always procedure that has already waited, holds automatic
// variables of its own at the call, and has no way to reach the end of its
// body -- the run ends at the call exactly as it would anywhere else.
module Top;
  logic clk = 0;
  int edges;
  int after_call;
  string seen;

  always #1 clk = ~clk;

  always @(posedge clk) begin
    automatic string note = $sformatf("edge %0d", edges);
    edges = edges + 1;
    seen = note;
    if (edges == 3) $finish;
    after_call = edges;
  end

  final begin
    if (edges !== 3) $fatal(1, "edges was %0d, expected 3", edges);
    if (after_call !== 2)
      $fatal(1, "after_call was %0d, expected 2", after_call);
    if (seen != "edge 2")
      $fatal(1, "seen was \"%s\", expected \"edge 2\"", seen);
    $display("All checks passed");
  end
endmodule
