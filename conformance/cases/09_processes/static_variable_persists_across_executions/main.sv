// A variable of static lifetime has one unique location, and leaving or
// entering the block that declares it does not affect the value stored in it
// (LRM 9.3.4); its initialization value expression is applied once before any
// process starts rather than on each entry to the block (LRM 6.21). A count
// kept in a static variable declared inside an always procedure therefore
// carries across that procedure's repeated executions rather than starting
// over each time the procedure is triggered.
module Top;
  bit clk;
  int seen[3];
  int edges;

  initial repeat (6) #5 clk = ~clk;

  always @(posedge clk) begin
    static int count = 0;
    count = count + 1;
    seen[edges] = count;
    edges = edges + 1;
  end

  final begin
    if (edges !== 3)
      $fatal(1, "edges was %0d, expected 3", edges);
    if (seen[0] !== 1)
      $fatal(1, "seen[0] was %0d, expected 1", seen[0]);
    if (seen[1] !== 2)
      $fatal(1, "seen[1] was %0d, expected 2", seen[1]);
    if (seen[2] !== 3)
      $fatal(1, "seen[2] was %0d, expected 3", seen[2]);
    $display("All checks passed");
  end
endmodule
