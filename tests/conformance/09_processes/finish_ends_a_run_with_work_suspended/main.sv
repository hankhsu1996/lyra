// $finish ends the simulation whatever the live processes are doing (LRM
// 20.2), and a final procedure occurs at that end (LRM 9.2.3). Work suspended
// part way through is one of the things they can be doing: a task enable that
// has not returned and a delay that has not elapsed both simply stop where they
// are, so what they would have written is not written and the values the run
// did settle are what the final procedure observes.
module Top;
  int stepped;
  int late;
  int ended_at;

  task advance(input int amount);
    stepped = stepped + 1;
    #amount;
    stepped = stepped + 100;
  endtask

  initial begin
    stepped = 0;
    late = 7;
    advance(100);
    late = 1;
  end

  initial begin
    #100;
    late = 2;
  end

  initial begin
    ended_at = -1;
    #4;
    ended_at = $time;
    $finish;
  end

  final begin
    if (stepped !== 1)
      $fatal(
          1, "stepped was %0d, expected 1: the enable ran past its delay",
          stepped);
    if (late !== 7)
      $fatal(1, "late was %0d, expected 7: a suspended procedure resumed", late);
    if (ended_at !== 4)
      $fatal(1, "the run ended at %0d, expected 4", ended_at);
    $display("All checks passed");
  end
endmodule
