// A delay control reached inside the body of an inner loop delays only the
// statement following it with respect to the one preceding it (LRM 9.4.1), so
// every piece of the enclosing state is still there when the process resumes.
// The outer and the inner loop variable are each automatic and local to the
// implicit block their own loop creates (LRM 12.7.1, 6.21), and each still
// holds the count of the pass that suspended. A variable declared automatic
// in the outer loop's body is initialized on entry to that body, so it
// accumulates across the whole inner loop and starts over on the next outer
// pass (LRM 6.21). A delay inside one arm of a conditional resumes in that
// arm, the arm having been fixed when the predicate was evaluated (LRM 12.4).
module Top;
  timeunit 1ns;
  timeprecision 1ns;

  int trace[9] = '{-1, -1, -1, -1, -1, -1, -1, -1, -1};
  int per_outer[3] = '{-1, -1, -1};
  int arms[3];
  time end_time;

  initial begin
    for (int outer = 0; outer < 3; outer++) begin
      automatic int running = 0;
      for (int inner = 1; inner <= 3; inner++) begin
        automatic int step = outer * 10 + inner;
        if (inner == 2) begin
          #7;
          running = running + step * 2;
          arms[outer] = arms[outer] * 10 + 2;
        end else begin
          #3;
          running = running + step;
          arms[outer] = arms[outer] * 10 + 1;
        end
        trace[outer * 3 + inner - 1] = running;
      end
      per_outer[outer] = running;
    end
    end_time = $time;
  end

  final begin
    if (trace[0] !== 1) $fatal(1, "trace[0] was %0d, expected 1", trace[0]);
    if (trace[1] !== 5) $fatal(1, "trace[1] was %0d, expected 5", trace[1]);
    if (trace[2] !== 8) $fatal(1, "trace[2] was %0d, expected 8", trace[2]);
    if (trace[3] !== 11) $fatal(1, "trace[3] was %0d, expected 11", trace[3]);
    if (trace[4] !== 35) $fatal(1, "trace[4] was %0d, expected 35", trace[4]);
    if (trace[5] !== 48) $fatal(1, "trace[5] was %0d, expected 48", trace[5]);
    if (trace[6] !== 21) $fatal(1, "trace[6] was %0d, expected 21", trace[6]);
    if (trace[7] !== 65) $fatal(1, "trace[7] was %0d, expected 65", trace[7]);
    if (trace[8] !== 88) $fatal(1, "trace[8] was %0d, expected 88", trace[8]);
    if (per_outer[0] !== 8)
      $fatal(1, "per_outer[0] was %0d, expected 8", per_outer[0]);
    if (per_outer[1] !== 48)
      $fatal(1, "per_outer[1] was %0d, expected 48", per_outer[1]);
    if (per_outer[2] !== 88)
      $fatal(1, "per_outer[2] was %0d, expected 88", per_outer[2]);
    if (arms[0] !== 121)
      $fatal(1, "the first outer pass took arms %0d, expected 121", arms[0]);
    if (arms[1] !== 121)
      $fatal(1, "the second outer pass took arms %0d, expected 121", arms[1]);
    if (arms[2] !== 121)
      $fatal(1, "the third outer pass took arms %0d, expected 121", arms[2]);
    if (end_time !== 39)
      $fatal(1, "the nested loop finished at %0d, expected 39", end_time);
    $display("All checks passed");
  end
endmodule
