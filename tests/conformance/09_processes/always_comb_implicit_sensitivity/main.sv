// The implicit sensitivity list of an always_comb includes every net or
// variable read within the block or within any function called within the
// block, so a change to any of them after time zero re-triggers the procedure
// (LRM 9.2.2.2.1). Nothing narrows that to the procedure's own scope: a signal
// named through a hierarchical path, in either order of declaration, and a
// signal a called function reads without taking it as an argument are read by
// the block like any other (LRM 9.2.2.2.1, 9.2.2.2.2).
module Top;
  int local_a;
  int local_b;
  int local_sum;

  logic [7:0] enclosing;
  logic [7:0] from_function;

  function automatic logic [7:0] read_enclosing();
    return enclosing;
  endfunction

  always_comb local_sum = local_a + local_b;
  always_comb from_function = read_enclosing();

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

  initial begin
    local_a = 1;
    local_b = 2;
    enclosing = 8'd0;
    src.v = 8'd0;
    p.sig = 8'd0;
    q.sig = 8'd0;
    #1;
    local_a = 3;
    enclosing = 8'd11;
    src.v = 8'd9;
    p.sig = 8'd3;
    q.sig = 8'd4;
    #1;
  end

  final begin
    if (local_sum !== 5) $fatal(1, "local_sum was %0d, expected 5", local_sum);
    if (from_function !== 8'd11)
      $fatal(1, "from_function was %0d, expected 11", from_function);
    if (rdr.o !== 8'd9) $fatal(1, "rdr.o was %0d, expected 9", rdr.o);
    if (p.got !== 8'd4) $fatal(1, "p.got was %0d, expected 4", p.got);
    if (q.got !== 8'd3) $fatal(1, "q.got was %0d, expected 3", q.got);
    $display("All checks passed");
  end
endmodule
