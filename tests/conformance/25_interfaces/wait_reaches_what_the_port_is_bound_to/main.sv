// A name through an interface port reaches the interface instance that port is
// connected to (LRM 25.3), so two instances of one module connected to two
// interface instances wait on two different things by the same text. A change
// in one interface wakes the module connected to it and not the other -- for an
// event control, a member of a nested interface and of an element of a nested
// array of them (LRM 25.10), `always_comb` and `@*`, a `wait`, a function the
// procedure calls, a name a modport lists and one it defines (LRM 25.5.4), a
// continuous assignment and a net declaration assignment, a connection to a
// child's input, and a sampled value function.
//
// Every counter starts counting after time zero, where which of a procedure and
// the assignment that first wakes it runs first is not determined.
interface Inner;
  logic [7:0] mark = 8'h00;
endinterface

interface Outer;
  logic [7:0] own = 8'h00;
  logic clk = 1'b0;
  Inner inner ();
  Inner bank[2] ();
  modport view(input own, input .twice(own + own));
endinterface

module Sink (
    input logic [7:0] d
);
  int woken = 0;
  always @(d) if ($time > 0) woken++;
endmodule

module Leaf (
    Outer o,
    Outer.view v
);
  int by_event = 0;
  int by_nested = 0;
  int by_bank = 0;
  int by_comb = 0;
  int by_star = 0;
  int by_wait = 0;
  int by_call = 0;
  int by_view = 0;
  int by_view_name = 0;
  int by_assign = 0;
  int by_net = 0;
  int by_rose = 0;
  logic [7:0] seen_comb, seen_star, seen_call;
  wire [7:0] assigned;
  wire [7:0] declared = o.own;

  function automatic logic [7:0] Through();
    return o.own;
  endfunction

  always @(o.own) by_event++;
  always @(o.inner.mark) by_nested++;
  always @(o.bank[1].mark) by_bank++;
  always_comb begin
    seen_comb = o.own;
    if ($time > 0) by_comb++;
  end
  always @* begin
    seen_star = o.own;
    by_star++;
  end
  initial begin
    wait (o.own == 8'h02);
    by_wait++;
  end
  always_comb begin
    seen_call = Through();
    if ($time > 0) by_call++;
  end
  always @(v.own) by_view++;
  always @(v.twice) by_view_name++;
  assign assigned = o.own;
  always @(assigned) if ($time > 0) by_assign++;
  always @(declared) if ($time > 0) by_net++;
  always @(posedge o.clk) if ($rose(o.own[0])) by_rose++;

  Sink sink (.d(o.own));
endmodule

module Top;
  Outer first ();
  Outer second ();

  Leaf on_first (
      first,
      first
  );
  Leaf on_second (
      second,
      second
  );

  initial begin
    #1 second.own = 8'h01;
    #1 second.clk = 1'b1;
    #1 second.own = 8'h02;
    #1 second.own = 8'h03;
    #1 second.inner.mark = 8'h01;
    #1 second.inner.mark = 8'h02;
    #1 second.bank[1].mark = 8'h01;
    #1 second.bank[1].mark = 8'h02;
    #1 first.own = 8'h10;
    #1 first.inner.mark = 8'h10;
    #1 first.bank[1].mark = 8'h10;
    #1 first.clk = 1'b1;
  end

  final begin
    if (on_second.by_event !== 3)
      $fatal(1, "on_second.by_event was %0d, expected 3", on_second.by_event);
    if (on_first.by_event !== 1)
      $fatal(1, "on_first.by_event was %0d, expected 1", on_first.by_event);
    if (on_second.by_nested !== 2)
      $fatal(1, "on_second.by_nested was %0d, expected 2", on_second.by_nested);
    if (on_first.by_nested !== 1)
      $fatal(1, "on_first.by_nested was %0d, expected 1", on_first.by_nested);
    if (on_second.by_bank !== 2)
      $fatal(1, "on_second.by_bank was %0d, expected 2", on_second.by_bank);
    if (on_first.by_bank !== 1)
      $fatal(1, "on_first.by_bank was %0d, expected 1", on_first.by_bank);
    if (on_second.by_comb !== 3)
      $fatal(1, "on_second.by_comb was %0d, expected 3", on_second.by_comb);
    if (on_first.by_comb !== 1)
      $fatal(1, "on_first.by_comb was %0d, expected 1", on_first.by_comb);
    if (on_second.seen_comb !== 8'h03)
      $fatal(1, "on_second.seen_comb was %h, expected 03", on_second.seen_comb);
    if (on_second.by_star !== 3)
      $fatal(1, "on_second.by_star was %0d, expected 3", on_second.by_star);
    if (on_first.by_star !== 1)
      $fatal(1, "on_first.by_star was %0d, expected 1", on_first.by_star);
    if (on_second.seen_star !== 8'h03)
      $fatal(1, "on_second.seen_star was %h, expected 03", on_second.seen_star);
    if (on_second.by_wait !== 1)
      $fatal(1, "on_second.by_wait was %0d, expected 1", on_second.by_wait);
    if (on_first.by_wait !== 0)
      $fatal(1, "on_first.by_wait was %0d, expected 0", on_first.by_wait);
    if (on_second.by_call !== 3)
      $fatal(1, "on_second.by_call was %0d, expected 3", on_second.by_call);
    if (on_first.by_call !== 1)
      $fatal(1, "on_first.by_call was %0d, expected 1", on_first.by_call);
    if (on_second.seen_call !== 8'h03)
      $fatal(1, "on_second.seen_call was %h, expected 03", on_second.seen_call);
    if (on_second.by_view !== 3)
      $fatal(1, "on_second.by_view was %0d, expected 3", on_second.by_view);
    if (on_first.by_view !== 1)
      $fatal(1, "on_first.by_view was %0d, expected 1", on_first.by_view);
    if (on_second.by_view_name !== 3)
      $fatal(1, "on_second.by_view_name was %0d, expected 3", on_second.by_view_name);
    if (on_first.by_view_name !== 1)
      $fatal(1, "on_first.by_view_name was %0d, expected 1", on_first.by_view_name);
    if (on_second.by_assign !== 3)
      $fatal(1, "on_second.by_assign was %0d, expected 3", on_second.by_assign);
    if (on_first.by_assign !== 1)
      $fatal(1, "on_first.by_assign was %0d, expected 1", on_first.by_assign);
    if (on_second.by_net !== 3)
      $fatal(1, "on_second.by_net was %0d, expected 3", on_second.by_net);
    if (on_first.by_net !== 1)
      $fatal(1, "on_first.by_net was %0d, expected 1", on_first.by_net);
    if (on_second.sink.woken !== 3)
      $fatal(1, "on_second.sink.woken was %0d, expected 3", on_second.sink.woken);
    if (on_first.sink.woken !== 1)
      $fatal(1, "on_first.sink.woken was %0d, expected 1", on_first.sink.woken);
    if (on_second.by_rose !== 1)
      $fatal(1, "on_second.by_rose was %0d, expected 1", on_second.by_rose);
    if (on_first.by_rose !== 0)
      $fatal(1, "on_first.by_rose was %0d, expected 0", on_first.by_rose);
    $display("All checks passed");
  end
endmodule
