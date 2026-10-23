// An index key names the element its value lands at, whichever way the
// dimension runs, and that holds as much for a pattern evaluated as a constant
// expression as for one assigned while the design runs. A parameter, a
// package parameter, a constant whose type is an array of an enum, and a
// variable's declaration each take a keyed pattern over a descending and an
// ascending dimension, packed and unpacked, with and without a default, and
// are read by a constant index, by a loop's index and from an `always_comb`
// (LRM 10.9.1, 6.20).
package Tables;
  localparam logic [2:0][1:0] KIND_OF = '{2: 2'd2, 1: 2'd1, 0: 2'd0};
endpackage

module Top;
  typedef enum logic [1:0] {IDLE, LOAD, STORE, FENCE} op_t;

  localparam logic [2:0][1:0] DESCENDING = '{2: 2'd2, 1: 2'd1, 0: 2'd0};
  localparam logic [0:2][1:0] ASCENDING = '{2: 2'd2, 1: 2'd1, 0: 2'd0};
  localparam logic [3:0][7:0] SOME_KEYED = '{1: 8'd20, 3: 8'd40, default: 8'd99};
  localparam int UNPACKED_DESCENDING [2:0] = '{0: 10, 1: 20, 2: 30};
  localparam int UNPACKED_ASCENDING [0:2] = '{0: 10, 1: 20, 2: 30};
  localparam op_t [3:0] OP_AT = '{3: FENCE, 2: STORE, 1: LOAD, 0: IDLE};

  logic [2:0][1:0] declared = '{2: 2'd2, 1: 2'd1, 0: 2'd0};
  logic [1:0] read_in_always_comb;
  int index;

  always_comb read_in_always_comb = DESCENDING[index];

  initial begin
    index = 0;
    #1;
    if (read_in_always_comb !== 2'd0)
      $fatal(1, "DESCENDING[0] read in always_comb was %0d, expected 0",
             read_in_always_comb);
    index = 2;
    #1;
    if (read_in_always_comb !== 2'd2)
      $fatal(1, "DESCENDING[2] read in always_comb was %0d, expected 2",
             read_in_always_comb);
  end

  final begin
    if (DESCENDING !== 6'b10_01_00)
      $fatal(1, "DESCENDING was %b, expected 100100", DESCENDING);
    if (ASCENDING !== 6'b00_01_10)
      $fatal(1, "ASCENDING was %b, expected 000110", ASCENDING);
    if (Tables::KIND_OF !== 6'b10_01_00)
      $fatal(1, "Tables::KIND_OF was %b, expected 100100", Tables::KIND_OF);
    if (SOME_KEYED !== 32'h28_63_14_63)
      $fatal(1, "SOME_KEYED was %h, expected 28631463", SOME_KEYED);
    if (declared !== 6'b10_01_00)
      $fatal(1, "declared was %b, expected 100100", declared);

    for (int i = 0; i <= 2; i++) begin
      if (DESCENDING[i] !== i)
        $fatal(1, "DESCENDING[%0d] was %0d", i, DESCENDING[i]);
      if (ASCENDING[i] !== i)
        $fatal(1, "ASCENDING[%0d] was %0d", i, ASCENDING[i]);
      if (Tables::KIND_OF[i] !== i)
        $fatal(1, "Tables::KIND_OF[%0d] was %0d", i, Tables::KIND_OF[i]);
      if (UNPACKED_DESCENDING[i] !== 10 * (i + 1))
        $fatal(1, "UNPACKED_DESCENDING[%0d] was %0d", i, UNPACKED_DESCENDING[i]);
      if (UNPACKED_ASCENDING[i] !== 10 * (i + 1))
        $fatal(1, "UNPACKED_ASCENDING[%0d] was %0d", i, UNPACKED_ASCENDING[i]);
    end

    if (OP_AT[0] !== IDLE || OP_AT[1] !== LOAD || OP_AT[2] !== STORE ||
        OP_AT[3] !== FENCE)
      $fatal(1, "OP_AT was %p, expected IDLE LOAD STORE FENCE by index", OP_AT);
    if (SOME_KEYED[0] !== 8'd99 || SOME_KEYED[1] !== 8'd20 ||
        SOME_KEYED[3] !== 8'd40)
      $fatal(1, "SOME_KEYED's elements were %0d %0d %0d %0d", SOME_KEYED[3],
             SOME_KEYED[2], SOME_KEYED[1], SOME_KEYED[0]);
    $display("All checks passed");
  end
endmodule
