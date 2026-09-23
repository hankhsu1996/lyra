// A conditional generate construct selects at most one of its alternative
// generate blocks from a constant expression evaluated during elaboration, and
// the selected block, if any, is instantiated into the model (LRM 27.5). The
// initial procedure in a selected block therefore runs, the one in an
// alternative that was not selected is not part of the design at all, and a
// construct whose condition fails contributes its else block or nothing. Two
// initial procedures in one time step execute in arbitrary order (LRM 4.7), so
// what each one did is observable but the order in which they did it is not.
//
// The same rule holds where the construct stands inside a loop generate, and
// that position is the one that composes differently: the condition reads the
// loop index, which is a different value in every block the loop counts out
// (LRM 27.4), so one construct selects a different alternative at different
// indices. Each index gets the alternative its own condition selected, the
// block keeps the name the source gave it whichever alternative stood, and a
// hierarchical name reaches into the one that did.
module Top;
  localparam int N = 6;

  bit top_ran;
  bit child_ran;
  bit else_ran;

  int chain [N];
  int seeded;
  int stepped;
  int ranked [N];
  int keyed [N];
  int sparse [N];

  localparam int P = 2;
  int nested;
  int deep [N];

  if (1) begin : g_taken
    initial child_ran = 1;
  end

  if (0) begin : g_untaken
    initial $fatal(1, "a generate block whose condition failed was elaborated");
  end

  if (0) begin : g_wrong_arm
    initial $fatal(1, "the unselected arm of a conditional generate ran");
  end else begin : g_other_arm
    initial else_ran = 1;
  end

  for (genvar i = 0; i < N; i++) begin : g
    if (i == 0) begin : arm
      int mark;
      initial begin
        mark = 100;
        chain[i] = 7;
      end
    end else begin : arm
      int mark;
      initial begin
        mark = 200 + i;
        chain[i] = i * 10;
      end
    end
  end

  // An `else if` chain is one construct with three alternatives rather than
  // nested ones, and the blocks of the directly nested conditional belong to
  // the outer one -- which is why all three may carry the same name (LRM 27.5).
  for (genvar i = 0; i < N; i++) begin : r
    if (i == 0) begin : arm
      initial ranked[i] = 100;
    end else if (i == N - 1) begin : arm
      initial ranked[i] = 900;
    end else begin : arm
      initial ranked[i] = 500;
    end
  end

  // A `case` compares its own expression against each item's labels in the
  // order they are written, and takes the default only where every comparison
  // failed (LRM 12.5).
  for (genvar i = 0; i < N; i++) begin : c
    case (i)
      0: begin : arm
        initial keyed[i] = 10;
      end
      1, 2: begin : arm
        initial keyed[i] = 20;
      end
      default: begin : arm
        initial keyed[i] = 30;
      end
    endcase
  end

  // A conditional whose condition fails and that wrote no `else` contributes
  // nothing at that index, so the array it writes keeps its initial value
  // there.
  for (genvar i = 0; i < N; i++) begin : s
    if (i > 2) begin : arm
      initial sparse[i] = i;
    end
  end

  // A conditional written inside another's taken side belongs to the outer
  // construct (LRM 27.5), and what selects one of its blocks is its own
  // condition *and* the outer one having held. Here the outer condition fails,
  // so neither arm of the inner `case` is instantiated -- its `default`
  // included -- and what runs is the outer `else`.
  if (P == 1)
    case (P)
      1: begin : n
        initial nested = 1;
      end
      default: begin : n
        initial nested = 2;
      end
    endcase
  else begin : n
    initial nested = 99;
  end

  // The same nesting inside a loop, which is the position that composes
  // differently: the outer condition reads the index, so what selects each of
  // the inner alternatives is that condition and its own label, and a name for
  // either has to say both.
  for (genvar i = 0; i < N; i++) begin : d
    if (i < 3)
      case (i)
        0: begin : arm
          initial deep[i] = 1;
        end
        default: begin : arm
          initial deep[i] = 2;
        end
      endcase
    else begin : arm
      initial deep[i] = 3;
    end
  end

  initial top_ran = 1;

  final begin
    if (!top_ran) $fatal(1, "the module's initial procedure did not run");
    if (!child_ran)
      $fatal(1, "the generate block's initial procedure did not run");
    if (!else_ran)
      $fatal(1, "the else block's initial procedure did not run");
    if (chain[0] !== 7)
      $fatal(1, "index 0 took the wrong alternative and wrote %0d", chain[0]);
    for (int k = 1; k < N; k++) begin
      if (chain[k] !== k * 10)
        $fatal(1, "index %0d wrote %0d, expected %0d", k, chain[k], k * 10);
    end
    seeded = g[0].arm.mark;
    stepped = g[3].arm.mark;
    if (seeded !== 100)
      $fatal(1, "g[0].arm.mark is %0d, expected 100", seeded);
    if (stepped !== 203)
      $fatal(1, "g[3].arm.mark is %0d, expected 203", stepped);
    for (int k = 0; k < N; k++) begin
      automatic int want_rank = (k == 0) ? 100 : (k == N - 1) ? 900 : 500;
      automatic int want_key = (k == 0) ? 10 : (k <= 2) ? 20 : 30;
      if (ranked[k] !== want_rank)
        $fatal(1, "chain index %0d wrote %0d, expected %0d", k, ranked[k],
               want_rank);
      if (keyed[k] !== want_key)
        $fatal(1, "case index %0d took the item writing %0d, expected %0d", k,
               keyed[k], want_key);
    end
    for (int k = 0; k <= 2; k++) begin
      if (sparse[k] !== 0)
        $fatal(1, "index %0d has no block, yet %0d was written", k, sparse[k]);
    end
    for (int k = 3; k < N; k++) begin
      if (sparse[k] !== k)
        $fatal(1, "index %0d wrote %0d, expected %0d", k, sparse[k], k);
    end
    if (nested !== 99)
      $fatal(1, "the nested conditional wrote %0d, expected 99", nested);
    for (int k = 0; k < N; k++) begin
      automatic int want_deep = (k == 0) ? 1 : (k < 3) ? 2 : 3;
      if (deep[k] !== want_deep)
        $fatal(1, "nested index %0d wrote %0d, expected %0d", k, deep[k],
               want_deep);
    end
    $display("All checks passed");
  end
endmodule
