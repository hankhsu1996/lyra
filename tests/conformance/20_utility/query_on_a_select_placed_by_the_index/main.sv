// A query on an indexed part-select whose position a loop index, or a
// parameter, gives. No clause numbers the bits of what a part-select yields
// (LRM 11.5.1, 11.8.1), but the select has one type, so a query on it answers
// alike where a constant is required -- a declaration's range, a local
// parameter -- and where the program runs, in every block and every instance
// (LRM 20.6.1, 20.7).
module Query #(parameter int B = 0) (output logic ok);
  logic [15:0] a;
  logic [$left(a[B*8+:8]):$right(a[B*8+:8])] w;
  localparam string Name = $typename(a[B*8+:8]);
  initial
    ok = $left(a[B*8+:8]) == $left(w) && $right(a[B*8+:8]) == $right(w) &&
         $low(a[B*8+:8]) == $low(w) && $size(a[B*8+:8]) == 8 &&
         $typename(a[B*8+:8]) == Name;
endmodule

module Top;
  logic [15:0] a;
  logic ok[2];
  logic ok0, ok1;

  for (genvar b = 0; b < 2; b++) begin : g
    logic [$left(a[b*8+:8]):$right(a[b*8+:8])] w;
    localparam string Name = $typename(a[b*8+:8]);
    initial
      ok[b] = $left(a[b*8+:8]) == $left(w) && $right(a[b*8+:8]) == $right(w) &&
              $size(a[b*8+:8]) == 8 && $typename(a[b*8+:8]) == Name;
  end

  Query #(.B(0)) q0 (.ok(ok0));
  Query #(.B(1)) q1 (.ok(ok1));

  initial begin
    #1;
    if (ok[0] !== 1'b1 || ok[1] !== 1'b1) $fatal(1, "blocks answered %b and %b", ok[0], ok[1]);
    if (ok0 !== 1'b1 || ok1 !== 1'b1) $fatal(1, "instances answered %b and %b", ok0, ok1);
    $display("All checks passed");
  end
endmodule
