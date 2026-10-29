// A block event naming the end of a named block samples immediately after the
// block executes its last statement, and not where the block is disabled (LRM
// 19.3). Whether a `return`, `break` or `continue` that jumps out of a block
// is the last statement it executes the standard does not say. Lyra takes it
// as the last, so a block left that way ends there and samples. This case
// holds Lyra to that.
module Top;
  bit [1:0] v;

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

  covergroup at_inner @@(end t3.inner);
    coverpoint v { bins b[] = {[0:3]}; }
  endgroup
  covergroup at_round @@(end t4.round);
    coverpoint v { bins b[] = {[0:3]}; }
  endgroup

  at_inner gi = new;
  at_round gr = new;
  real inner_cov, round_cov;

  initial begin
    v = 0;
    t3(1);
    t3(2);
    t4(0);
    inner_cov = gi.get_inst_coverage();
    round_cov = gr.get_inst_coverage();
  end

  final begin
    // The block a `return` left ended at 1, the one that ran on at 3.
    if (inner_cov != 50.0) $fatal(1, "the returned-from block's coverage was %0f, expected 50", inner_cov);
    // The round a `continue` left ended at 0, the one a `break` left at 1.
    if (round_cov != 50.0) $fatal(1, "the loop body's coverage was %0f, expected 50", round_cov);
    $display("All checks passed");
  end
endmodule
