// A covergroup with a clocking event samples the instant the event takes place,
// as if the process triggering it called `sample`, as many times as it occurs
// in a time step; with `strobe` it samples once a time slot, in its Postponed
// region (LRM 19.3, 19.7).
module Top;
  bit clk;
  bit [1:0] v;
  event e;

  covergroup on_edge @(posedge clk);
    cp: coverpoint v;
  endgroup

  covergroup on_event @(e);
    cp: coverpoint v { bins b[] = {[0:3]}; }
  endgroup

  covergroup strobed @(e);
    type_option.strobe = 1;
    cp: coverpoint v { bins b[] = {[0:3]}; }
  endgroup

  on_edge module_level = new;
  on_event each = new;
  strobed once = new;
  real edge_cov, each_cov, once_cov;

  initial begin
    static on_edge local_one = new;
    repeat (3) begin
      #1 clk = 1;
      #1 clk = 0;
      v++;
    end
    #1;
    // Each trigger samples the value as it stands there: 1 and 2. The strobed
    // one samples the slot's last value, 3, once.
    v = 1; ->e; v = 2; ->e; v = 3;
    #1;
    edge_cov = local_one.get_inst_coverage();
    if (module_level.get_inst_coverage() != edge_cov)
      $fatal(1, "two instances sampled on one edge disagree");
    each_cov = each.get_inst_coverage();
    once_cov = once.get_inst_coverage();
  end

  final begin
    // 0, 1 and 2 of 4 automatic bins.
    if (edge_cov != 75.0) $fatal(1, "the edge's coverage was %0f, expected 75", edge_cov);
    if (each_cov != 50.0) $fatal(1, "each trigger's coverage was %0f, expected 50", each_cov);
    if (once_cov != 25.0) $fatal(1, "the strobed coverage was %0f, expected 25", once_cov);
    $display("All checks passed");
  end
endmodule
