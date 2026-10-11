// A conditional generate selects at most one of its blocks on a constant
// expression (LRM 27.5), and a loop generate counts out a block for each value
// its header steps through (LRM 27.4). Where that expression reads a parameter
// of the module, every instance overriding the parameter (LRM 23.10) builds
// what its own value says: one module is given four values here, and each
// instance holds the alternative, the case item and the number of blocks its
// value selects.
//
// A name reaching into a selected block reaches the block that instance
// built (LRM 23.6), whichever alternative another instance of the module
// selected, and a block of one alternative may carry the name of another's
// because at most one of them stands.
//
// The value may reach the condition through a parameter the module declares
// from it (LRM 6.20.4), and the same parameter may be read as a value inside
// the block it selected.
module Built #(
    parameter int P = 0
) (
    input logic [7:0] a,
    output logic [7:0] y
);
  localparam bit ENABLED = P != 0;

  int counted;
  int keyed;

  if (P != 0) begin : arm
    int mark = 100 + P;
    assign y = a + 8'(P);
  end else begin : arm
    int mark = 7;
    assign y = '0;
  end

  if (ENABLED) begin : enabled
    int seen = P;
  end

  case (P)
    0: begin : item
      initial keyed = 10;
    end
    1, 2: begin : item
      initial keyed = 20;
    end
    default: begin : item
      initial keyed = 30;
    end
  endcase

  for (genvar i = 0; i < P; i++) begin : stage
    int place = i * 10 + P;
    initial counted = counted + 1;
  end

  for (genvar i = 0; i < 2; i++) begin : fixed
    if (P > i) begin : above
      int level = i + P;
    end
  end
endmodule

module Top;
  logic [7:0] a;
  logic [7:0] y0, y1, y2, y3;

  Built #(.P(0)) none (.a(a), .y(y0));
  Built #(.P(1)) one (.a(a), .y(y1));
  Built #(.P(2)) two (.a(a), .y(y2));
  Built #(.P(3)) three (.a(a), .y(y3));

  initial begin
    a = 8'd40;
    #1;
    if (y0 !== 8'd0) $fatal(1, "P=0 drives %0d, expected 0", y0);
    if (y1 !== 8'd41) $fatal(1, "P=1 drives %0d, expected 41", y1);
    if (y2 !== 8'd42) $fatal(1, "P=2 drives %0d, expected 42", y2);
    if (y3 !== 8'd43) $fatal(1, "P=3 drives %0d, expected 43", y3);

    if (none.arm.mark !== 7)
      $fatal(1, "none.arm.mark is %0d, expected 7", none.arm.mark);
    if (one.arm.mark !== 101)
      $fatal(1, "one.arm.mark is %0d, expected 101", one.arm.mark);
    if (three.arm.mark !== 103)
      $fatal(1, "three.arm.mark is %0d, expected 103", three.arm.mark);

    if (one.enabled.seen !== 1)
      $fatal(1, "one.enabled.seen is %0d, expected 1", one.enabled.seen);
    if (two.enabled.seen !== 2)
      $fatal(1, "two.enabled.seen is %0d, expected 2", two.enabled.seen);

    if (none.keyed !== 10) $fatal(1, "P=0 took item %0d", none.keyed);
    if (one.keyed !== 20) $fatal(1, "P=1 took item %0d", one.keyed);
    if (two.keyed !== 20) $fatal(1, "P=2 took item %0d", two.keyed);
    if (three.keyed !== 30) $fatal(1, "P=3 took item %0d", three.keyed);

    if (none.counted !== 0)
      $fatal(1, "P=0 counted out %0d blocks", none.counted);
    if (one.counted !== 1) $fatal(1, "P=1 counted out %0d blocks", one.counted);
    if (three.counted !== 3)
      $fatal(1, "P=3 counted out %0d blocks", three.counted);
    if (one.stage[0].place !== 1)
      $fatal(1, "one.stage[0].place is %0d, expected 1", one.stage[0].place);
    if (three.stage[2].place !== 23)
      $fatal(1, "three.stage[2].place is %0d, expected 23",
             three.stage[2].place);

    if (one.fixed[0].above.level !== 1)
      $fatal(1, "one.fixed[0].above.level is %0d, expected 1",
             one.fixed[0].above.level);
    if (three.fixed[1].above.level !== 4)
      $fatal(1, "three.fixed[1].above.level is %0d, expected 4",
             three.fixed[1].above.level);

    $display("All checks passed");
    $finish;
  end
endmodule
