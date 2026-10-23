// A block event samples immediately before a named block, task, function or
// method begins executing its first statement, or immediately after it
// executes its last; a `return`, `break` or `continue` that jumps out of a
// block is the last statement it executes, while a block left by `disable`
// does not end (LRM 19.3).
module Top;
  bit [1:0] v;

  task automatic work(bit [1:0] to);
    v = to;
  endtask

  task automatic t2(bit [1:0] to, bit leave_early);
    begin : blk
      v = to;
      if (leave_early) disable blk;
      v = to + 1;
    end
  endtask

  task automatic t3(bit [1:0] to);
    begin : inner
      v = to;
      if (to == 1) return;
      v = to + 1;
    end
  endtask

  task automatic t4(bit [1:0] to);
    int i = 0;
    while (i < 2) begin : round
      v = to + i;
      i++;
      if (i == 1) continue;
      break;
    end
  endtask

  covergroup at_begin @@(begin work);
    coverpoint v { bins b[] = {[0:3]}; }
  endgroup
  covergroup at_end @@(end work);
    coverpoint v { bins b[] = {[0:3]}; }
  endgroup
  covergroup at_blk @@(end t2.blk);
    coverpoint v { bins b[] = {[0:3]}; }
  endgroup
  covergroup at_inner @@(end t3.inner);
    coverpoint v { bins b[] = {[0:3]}; }
  endgroup
  covergroup at_round @@(end t4.round);
    coverpoint v { bins b[] = {[0:3]}; }
  endgroup

  class C;
    bit [1:0] m;
    covergroup cv @@(begin step);
      coverpoint m { bins b[] = {[0:3]}; }
    endgroup
    function new(); cv = new; endfunction
    function void step(); m = m + 1; endfunction
  endclass

  at_begin gb = new;
  at_end ge = new;
  at_blk gk = new;
  at_inner gi = new;
  at_round gr = new;
  real begin_cov, end_cov, blk_cov, inner_cov, round_cov, method_cov;

  initial begin
    static C c = new;
    v = 0;
    work(1);
    work(2);
    t2(2, 0);
    t2(0, 1);
    t3(1);
    t3(2);
    t4(0);
    c.step();
    c.step();
    begin_cov = gb.get_inst_coverage();
    end_cov = ge.get_inst_coverage();
    blk_cov = gk.get_inst_coverage();
    inner_cov = gi.get_inst_coverage();
    round_cov = gr.get_inst_coverage();
    method_cov = c.cv.get_inst_coverage();
  end

  final begin
    // Begins sample 0 and 1, ends 1 and 2.
    if (begin_cov != 50.0) $fatal(1, "the begin coverage was %0f, expected 50", begin_cov);
    if (end_cov != 50.0) $fatal(1, "the end coverage was %0f, expected 50", end_cov);
    // Only the block that ran to its end sampled, at 3.
    if (blk_cov != 25.0) $fatal(1, "the block's coverage was %0f, expected 25", blk_cov);
    // The block a `return` left ended at 1, the one that ran on at 3.
    if (inner_cov != 50.0) $fatal(1, "the returned-from block's coverage was %0f, expected 50", inner_cov);
    // The round a `continue` left ended at 0, the one a `break` left at 1.
    if (round_cov != 50.0) $fatal(1, "the loop body's coverage was %0f, expected 50", round_cov);
    // The method began with m at 0, then 1.
    if (method_cov != 50.0) $fatal(1, "the method's coverage was %0f, expected 50", method_cov);
    $display("All checks passed");
  end
endmodule
