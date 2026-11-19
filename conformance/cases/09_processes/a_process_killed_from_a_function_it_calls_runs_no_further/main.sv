// A subroutine call runs in its caller's thread (LRM 9.5), so a function an
// always procedure calls may kill that procedure's own process through
// process::self() (LRM 9.7). The process terminates there: the statements
// after the call do not run, it reports KILLED, and it is never woken by its
// event control again.
module Top;
  bit clk;
  process ff;
  int edges_seen;
  int ran_after_the_kill;

  function automatic void end_this_process();
    process::self().kill();
  endfunction

  always @(posedge clk) begin
    ff = process::self();
    edges_seen++;
    if (edges_seen == 2) begin
      end_this_process();
      ran_after_the_kill++;
    end
  end

  initial begin
    repeat (3) begin
      #1 clk = 1;
      #1 clk = 0;
    end
  end

  final begin
    if (edges_seen !== 2)
      $fatal(1, "edges_seen was %0d, expected 2", edges_seen);
    if (ran_after_the_kill !== 0)
      $fatal(1, "ran_after_the_kill was %0d, expected 0", ran_after_the_kill);
    if (ff.status() !== process::KILLED)
      $fatal(1, "the process reports state %0d, expected KILLED",
             ff.status());
    $display("All checks passed");
  end
endmodule
