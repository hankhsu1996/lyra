// LRM 9.2.2.2.1: an always_comb's inferred sensitivity covers every non-local
// signal it reads -- a sibling scope's signal named hierarchically, in either
// declaration order, and a signal read inside a function it calls -- and wakes
// the block when any of them changes. Every source changes after time zero, so
// a subscription that only settled at time zero, that dropped a non-enclosing
// target, or that saw only a function's arguments would miss the update.
//
//   p.got reads q.sig, with q declared after p; q.got reads p.sig, with p
//   declared before q -- both cross-scope directions and declaration orders.
//   fn_a reads an enclosing signal through a called function; fn_c reads a
//   sibling block's signal through a called function.
module Test;
  logic [7:0] a;

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

  if (1) begin : c
    logic [7:0] sig;
  end

  function automatic logic [7:0] read_a();
    return a;
  endfunction

  function automatic logic [7:0] read_c();
    return c.sig;
  endfunction

  logic [7:0] fn_a;
  logic [7:0] fn_c;
  always_comb fn_a = read_a();
  always_comb fn_c = read_c();

  initial begin
    a = 8'd0;
    p.sig = 8'd0;
    q.sig = 8'd0;
    c.sig = 8'd0;
    #1;
    a = 8'd1;
    p.sig = 8'd3;
    q.sig = 8'd4;
    c.sig = 8'd2;
    #1;
    $display("p=%0d q=%0d fn_a=%0d fn_c=%0d", p.got, q.got, fn_a, fn_c);
  end
endmodule
