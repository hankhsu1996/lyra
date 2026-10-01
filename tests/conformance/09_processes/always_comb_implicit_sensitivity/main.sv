// The implicit sensitivity list of an always_comb includes every net or
// variable read within the block or within any function called within the
// block, so a change to any of them after time zero re-triggers the procedure
// (LRM 9.2.2.2.1). Nothing narrows that to the procedure's own scope: a signal
// named through a hierarchical path, in either order of declaration, and a
// signal a called function reads without taking it as an argument are read by
// the block like any other (LRM 9.2.2.2.1, 9.2.2.2.2). A static class property
// is one of those variables -- LRM 8.9 makes it the single copy a class shares,
// usable with no object of that type -- and the exception 9.2.2.2.1 states is
// for a reference to a class object, which naming one through the class scope
// resolution operator is not. Where the class itself is declared changes
// nothing about that, so one outside any design unit and one inside the module
// are read alike.
//
// What is read within the block is a matter of its text. The clause's three
// exceptions say nothing of a branch a constant condition never takes, so a
// read there is in the list like any other -- under an `if`, a `case`, a
// conditional operator, and the operand a logical operator skips -- and the
// procedure runs again when it changes. A select indexed by a loop variable is
// not a static prefix (LRM 11.5.3), so a loop over part of a vector reads all
// of it.
class Shared;
  static int level = 0;
endclass

module Top;
  class Owned;
    static int level = 0;
  endclass

  int local_a;
  int local_b;
  int local_sum;

  logic [7:0] enclosing;
  logic [7:0] from_function;
  int from_shared;
  int from_owned;

  function automatic logic [7:0] read_enclosing();
    return enclosing;
  endfunction

  always_comb local_sum = local_a + local_b;
  always_comb from_function = read_enclosing();
  always_comb from_shared = Shared::level + 100;
  always_comb from_owned = Owned::level + 200;

  if (1) begin : src
    logic [7:0] v;
  end

  function automatic logic [7:0] read_src();
    return src.v;
  endfunction

  if (1) begin : rdr
    logic [7:0] o;
    always_comb o = read_src();
  end

  if (1) begin : p
    logic [7:0] sig;
    logic [7:0] got;
    always_comb got = q.sig;
  end

  if (1) begin : q
    logic [7:0] sig;
    logic [7:0] got;
    always_comb got = p.sig;
  end

  localparam int kNever = 0;
  logic under_if, under_case, under_conditional, under_skipped_operand;
  logic [3:0] looped;
  logic sink_if, sink_case, sink_conditional, sink_operand, sink_loop;
  int runs_if, runs_case, runs_conditional, runs_operand, runs_loop;
  int before_if, before_case, before_conditional, before_operand, before_loop;

  always_comb begin
    runs_if++;
    if (kNever == 1) sink_if = under_if;
    else sink_if = 1'b0;
  end

  always_comb begin
    runs_case++;
    case (kNever)
      1: sink_case = under_case;
      default: sink_case = 1'b0;
    endcase
  end

  always_comb begin
    runs_conditional++;
    sink_conditional = (kNever == 1) ? under_conditional : 1'b0;
  end

  always_comb begin
    runs_operand++;
    sink_operand = (kNever == 1) && under_skipped_operand;
  end

  always_comb begin
    runs_loop++;
    sink_loop = 1'b0;
    for (int j = 0; j < 2; j++) sink_loop ^= looped[j];
  end

  initial begin
    local_a = 1;
    local_b = 2;
    enclosing = 8'd0;
    src.v = 8'd0;
    p.sig = 8'd0;
    q.sig = 8'd0;
    under_if = 1'b0;
    under_case = 1'b0;
    under_conditional = 1'b0;
    under_skipped_operand = 1'b0;
    looped = 4'b0000;
    #1;
    before_if = runs_if;
    before_case = runs_case;
    before_conditional = runs_conditional;
    before_operand = runs_operand;
    before_loop = runs_loop;
    under_if = 1'b1;
    under_case = 1'b1;
    under_conditional = 1'b1;
    under_skipped_operand = 1'b1;
    looped = 4'b1000;
    local_a = 3;
    enclosing = 8'd11;
    src.v = 8'd9;
    p.sig = 8'd3;
    q.sig = 8'd4;
    Shared::level = 7;
    Owned::level = 8;
    #1;
  end

  final begin
    if (local_sum !== 5) $fatal(1, "local_sum was %0d, expected 5", local_sum);
    if (from_function !== 8'd11)
      $fatal(1, "from_function was %0d, expected 11", from_function);
    if (rdr.o !== 8'd9) $fatal(1, "rdr.o was %0d, expected 9", rdr.o);
    if (p.got !== 8'd4) $fatal(1, "p.got was %0d, expected 4", p.got);
    if (q.got !== 8'd3) $fatal(1, "q.got was %0d, expected 3", q.got);
    if (from_shared !== 107)
      $fatal(1, "from_shared was %0d, expected 107", from_shared);
    if (from_owned !== 208)
      $fatal(1, "from_owned was %0d, expected 208", from_owned);
    if (runs_if - before_if !== 1)
      $fatal(1, "a read under an if never taken ran the block %0d times, expected 1",
             runs_if - before_if);
    if (runs_case - before_case !== 1)
      $fatal(1, "a read under a case item never taken ran the block %0d times, expected 1",
             runs_case - before_case);
    if (runs_conditional - before_conditional !== 1)
      $fatal(1, "a read in a conditional's other side ran the block %0d times, expected 1",
             runs_conditional - before_conditional);
    if (runs_operand - before_operand !== 1)
      $fatal(1, "a read in a skipped operand ran the block %0d times, expected 1",
             runs_operand - before_operand);
    if (runs_loop - before_loop !== 1)
      $fatal(1, "a bit a loop does not reach ran the block %0d times, expected 1",
             runs_loop - before_loop);
    $display("All checks passed");
  end
endmodule
