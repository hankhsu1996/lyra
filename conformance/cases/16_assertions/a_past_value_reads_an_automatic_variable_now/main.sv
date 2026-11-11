// A sampled value function reads its operand where the call is written, as the
// sampled value of the tick it names: each static variable as of that tick, and
// each automatic variable as it is now (LRM 16.5.1, 16.9.3). So `$past(b[i])`
// in a loop is the past value of b's i-th bit for the `i` the loop is at now,
// as the standard's own example has it, and a value change function compares
// the i-th bit now with the i-th bit at the prior tick.
//
// The same holds of an element of every kind of array, of a member, and of a
// variable named through the hierarchy, in a procedure of the module and of a
// generate block alike.
//
// Before the ticks a read reaches exist, two clauses disagree on what an
// automatic variable contributes: LRM 16.9.3 answers with "the default sampled
// value of expression1", whose recursive definition gives an automatic its
// type's default, and LRM 16.5.1 says "the current value of the automatic
// variable is taken instead" of a past value. So every check here waits until
// the ticks it reaches have happened, where both readings agree.
//
// A sampled value function nested in another reads its own operand as of a
// tick before the tick the outer one names, on whichever clock each counts.
module Child(input bit clk);
  logic [3:0] v = 4'b1100;
  always @(posedge clk) v <= v + 4'd1;
endmodule

module Top;
  localparam logic [3:0] kStart = 4'b0110;
  typedef int four_t[4];
  typedef int ints_t[$];
  typedef struct {int f[4];} holder_t;

  bit clk;
  bit clk2;
  bit gate;
  logic [3:0] b = kStart;
  four_t arr = '{1, 2, 3, 4};
  int q[$] = '{5, 6, 7, 8};
  int da[] = '{9, 10, 11, 12};
  holder_t h = '{f: '{13, 14, 15, 16}};

  default clocking @(posedge clk); endclocking

  // What `b` was found holding at each tick of each clock, the latest last,
  // and what `b` was at each tick of `clk` where `gate` held.
  logic [3:0] seen[$];
  logic [3:0] seen_gated[$];
  logic [3:0] seen_c2[$];
  logic [3:0] inner_at[$];
  four_t arr_seen[$];
  ints_t q_seen[$];
  ints_t da_seen[$];
  holder_t h_seen[$];
  logic [3:0] v_seen[$];
  int checks = 0;
  int failures = 0;

  always #5 clk = !clk;
  // `clk2` rises at 2, 22, 42, ..., never in a step where `clk` does.
  initial begin
    #2 clk2 = 1;
    forever #10 clk2 = !clk2;
  end

  // Each value changes after the tick that sampled it.
  always @(posedge clk) begin
    b <= b + 4'd3;
    foreach (arr[k]) arr[k] <= arr[k] + 4;
    foreach (h.f[k]) h.f[k] <= h.f[k] + 4;
  end
  // A queue's and a dynamic array's elements take no nonblocking write, so
  // they change between two ticks instead.
  always @(negedge clk) begin
    gate = !gate;
    foreach (q[k]) q[k] = q[k] + 4;
    foreach (da[k]) da[k] = da[k] + 4;
  end

  always @(posedge clk2) seen_c2.push_back(b);

  function automatic logic past_bit(int k);
    return $past(b[k]);
  endfunction

  function automatic logic [3:0] back(int n);
    return seen[seen.size()-1-n];
  endfunction

  task automatic check(logic got, logic want, string what, int i);
    checks++;
    if (got !== want) begin
      failures++;
      $display("tick %0d, i=%0d: %s was %b, expected %b", seen.size(), i, what,
               got, want);
    end
  endtask

  always @(posedge clk) begin
    logic [3:0] gated_before;
    logic [3:0] inner_before;
    seen.push_back(b);
    if (gate) seen_gated.push_back(b);
    inner_at.push_back(seen_c2[$]);
    arr_seen.push_back(arr);
    q_seen.push_back(q);
    da_seen.push_back(da);
    h_seen.push_back(h);
    v_seen.push_back(c.v);
    // From the third tick on, every read below reaches ticks that happened.
    if (seen.size() > 2) begin
      gated_before = seen_gated[seen_gated.size()-1-(gate ? 1 : 0)];
      inner_before = inner_at[$-1];
      for (int i = 0; i < 4; i++) begin
        check($past(b[i]), back(1)[i], "$past(b[i])", i);
        check($past(b[i], 2), back(2)[i], "$past(b[i], 2)", i);
        check($past(b[i], 1, gate), gated_before[i], "$past(b[i], 1, gate)",
              i);
        check($rose(b[i]), b[i] === 1'b1 && back(1)[i] !== 1'b1, "$rose(b[i])",
              i);
        check($fell(b[i]), b[i] === 1'b0 && back(1)[i] !== 1'b0, "$fell(b[i])",
              i);
        check($stable(b[i]), b[i] === back(1)[i], "$stable(b[i])", i);
        check($changed(b[i]), b[i] !== back(1)[i], "$changed(b[i])", i);
        check($past(b[3-i]), back(1)[3-i], "$past(b[3-i])", i);
        check(past_bit(i), back(1)[i], "an automatic function's $past(b[k])",
              i);
        check($past($past(b[i])), back(2)[i], "$past($past(b[i]))", i);
        check($past($rose(b[i])), back(1)[i] === 1'b1 && back(2)[i] !== 1'b1,
              "$past($rose(b[i]))", i);
        check($past($past(b[i], 1, , @(posedge clk2)), 1, , @(posedge clk)),
              inner_before[i], "a $past on clk of a $past on clk2", i);
        check($past(i) == i, 1'b1, "$past(i)", i);
        check($past(arr[i]) == arr_seen[$-1][i], 1'b1, "$past(arr[i])", i);
        check($past(q[i]) == q_seen[$-1][i], 1'b1, "$past(q[i])", i);
        check($past(da[i]) == da_seen[$-1][i], 1'b1, "$past(da[i])", i);
        check($past(h.f[i]) == h_seen[$-1].f[i], 1'b1, "$past(h.f[i])", i);
        check($past(c.v[i]), v_seen[$-1][i], "$past(c.v[i])", i);
      end
    end
  end

  Child c(clk);

  // A procedure of a generate block keeps its own histories.
  for (genvar n = 0; n < 2; n++) begin : g
    logic [3:0] mine[$];
    always @(posedge clk) begin
      mine.push_back(b);
      if (mine.size() > 1)
        for (int i = 0; i < 4; i++)
          check($past(b[i]), mine[$-1][i], "a generate block's $past(b[i])",
                i);
    end
  end

  // An expect waits in its procedure, so the automatic it reads is the one of
  // the iteration that reached it.
  int expected_once = 0;
  initial begin
    for (int i = 0; i < 2; i++)
      expect (@(posedge clk) ##1 $past(b[i]) === back(1)[i]) expected_once++;
      else $display("expect failed at i=%0d", i);
  end

  initial #95 $finish;

  final begin
    if (checks != 7 * 18 * 4 + 8 * 2 * 4)
      $fatal(1, "%0d checks ran, expected %0d", checks, 7 * 18 * 4 + 8 * 2 * 4);
    if (expected_once != 2)
      $fatal(1, "the expect statements passed %0d times, expected 2",
             expected_once);
    if (failures != 0) $fatal(1, "%0d of %0d checks failed", failures, checks);
    $display("All checks passed");
  end
endmodule
