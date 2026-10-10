// A continuous assignment is a process that is also evaluated at time zero in
// order to propagate constant values, and a port connection is an implicit
// one (LRM 4.9.1, 4.9.6). A net or a variable holds its initial value until
// then (LRM 4.5), so what a driver gives it at time zero is a change like any
// later one: a procedure waiting on the name sees it, as an edge where the
// change is one. A register whose only reset is an asynchronous one tied to a
// constant is reset by exactly this change.
//
// The standard leaves the order of the processes of one time slot open (LRM
// 4.7). The order held here is the one a design can lean on either way: an
// always procedure is waiting before any continuous driver first evaluates,
// and an initial procedure starts once the drivers have, so it reads what a
// constant drives. An always_comb is triggered after all initial and always
// procedures have been started (LRM 9.2.2.2), so it sees what an initial
// procedure assigned at time zero.
//
// A variable's declaration initializer is the contrast. It is set before any
// procedure starts (LRM 6.8), so it is no change to anyone.
module EdgeOnPort (
    input logic r
);
  logic fell = 0;
  logic changed = 0;
  always @(negedge r) fell = 1;
  always @(r) changed = 1;
endmodule

module RiseOnPort (
    input logic r
);
  logic rose = 0;
  always @(posedge r) rose = 1;
endmodule

// Drives its output from inside, for a module beside it to wait on.
module Source (
    output logic o
);
  assign o = 1'b0;
endmodule

// An initial procedure reading a tied input at time zero, with no delay.
module ReadsAtStart (
    input int seed
);
  int read = -1;
  initial read = seed;
endmodule

// A register only its asynchronous reset can clear, with a clock that never
// runs.
module Register (
    input  logic clk,
    input  logic rst_n,
    input  logic d,
    output logic q
);
  always_ff @(posedge clk or negedge rst_n) begin
    if (!rst_n) q <= 1'b0;
    else q <= d;
  end
endmodule

module Top;
  wire         net_declared = 1'b0;
  wire         net_assigned;
  wire   [3:0] net_vector = 4'h5;
  logic        var_assigned;
  logic        var_for_port;
  logic        var_initialized = 1'b0;

  assign net_assigned = 1'b0;
  assign var_assigned = 1'b0;
  assign var_for_port = 1'b0;

  logic net_declared_fell = 0;
  logic net_assigned_fell = 0;
  logic net_vector_changed = 0;
  logic var_assigned_fell = 0;
  logic var_assigned_changed = 0;
  logic var_initialized_fell = 0;
  logic var_initialized_changed = 0;

  always @(negedge net_declared) net_declared_fell = 1;
  always @(negedge net_assigned) net_assigned_fell = 1;
  always @(net_vector) net_vector_changed = 1;
  always @(negedge var_assigned) var_assigned_fell = 1;
  always @(var_assigned) var_assigned_changed = 1;
  always @(negedge var_initialized) var_initialized_fell = 1;
  always @(var_initialized) var_initialized_changed = 1;

  logic net_read_at_start;
  logic var_read_at_start;
  initial begin
    net_read_at_start = net_assigned;
    var_read_at_start = var_assigned;
  end

  int set_at_start = 0;
  int comb_saw;
  int comb_runs = 0;
  initial set_at_start = 7;
  always_comb begin
    comb_saw  = set_at_start;
    comb_runs = comb_runs + 1;
  end

  ReadsAtStart reads_tied (.seed(10));

  // The driver and the procedure waiting on it need not be parent and child.
  wire link;
  Source beside (.o(link));
  EdgeOnPort fed_by_a_module_beside (.r(link));

  logic in_block_fell = 0;
  if (1) begin : block
    always @(negedge net_assigned) in_block_fell = 1;
  end

  // A concurrent assertion is clocked like an always procedure, so a clock a
  // constant drives ticks for it at time zero.
  wire  rises = 1'b1;
  logic assertion_ticked = 0;
  assert property (@(posedge rises) 1'b1) assertion_ticked = 1;

  EdgeOnPort tied_low (.r(1'b0));
  RiseOnPort tied_high (.r(1'b1));
  EdgeOnPort fed_by_variable (.r(var_for_port));
  EdgeOnPort fed_by_net (.r(net_assigned));

  logic q;
  Register only_reset_clears (
      .clk(1'b0),
      .rst_n(1'b0),
      .d(1'b1),
      .q(q)
  );

  task automatic expect_seen(input string what, input logic got);
    if (got !== 1'b1)
      $fatal(1, "%s: the first value was not seen as a change", what);
  endtask

  initial begin
    #1;
    expect_seen("a net with a declaration assignment", net_declared_fell);
    expect_seen("a net with a continuous assignment", net_assigned_fell);
    expect_seen("a vector net, by a level-sensitive wait", net_vector_changed);
    expect_seen("a variable with a continuous assignment", var_assigned_fell);
    expect_seen("a variable with a continuous assignment, by a level wait",
                var_assigned_changed);
    expect_seen("an input port tied to 0", tied_low.fell);
    expect_seen("an input port tied to 0, by a level wait", tied_low.changed);
    expect_seen("an input port tied to 1", tied_high.rose);
    expect_seen("an input port fed by an assigned variable",
                fed_by_variable.fell);
    expect_seen("an input port fed by an assigned net", fed_by_net.fell);
    expect_seen("an input port fed by a module beside it",
                fed_by_a_module_beside.fell);
    expect_seen("a procedure in a generate block", in_block_fell);
    expect_seen("a concurrent assertion's clock", assertion_ticked);

    if (q !== 1'b0)
      $fatal(1, "a register reset by a port tied to 0 held %b, expected 0", q);
    if (net_vector !== 4'h5)
      $fatal(1, "the vector net held %h, expected 5", net_vector);

    if (net_read_at_start !== 1'b0 || var_read_at_start !== 1'b0)
      $fatal(1, "an initial procedure read %b from a net and %b from a variable",
             net_read_at_start, var_read_at_start);
    if (reads_tied.read !== 10)
      $fatal(1, "an initial procedure read %0d from a tied input, expected 10",
             reads_tied.read);
    if (comb_saw !== 7 || comb_runs !== 1)
      $fatal(1, "always_comb saw %0d in %0d runs, expected 7 in 1", comb_saw,
             comb_runs);

    if (var_initialized_fell !== 1'b0 || var_initialized_changed !== 1'b0)
      $fatal(1, "a declaration initializer was seen as a change");

    $display("All checks passed");
  end
endmodule
