// An event control may name several events joined by `or` or separated by
// commas, which mean the same thing, and the procedure resumes when any one of
// them occurs; arming the same list again catches the next one (LRM 9.4.2.1).
// Each procedure is a thread of its own (LRM 9.5), so every procedure armed on
// an event resumes when that event occurs, not just one of them.
module Top;
  bit clk;
  logic reset_n = 1'b1;
  logic [7:0] bus = 8'b0000_1000;

  time first_wake;
  time second_wake;
  time third_wake;
  time one_waiter_at;
  time other_waiter_at;

  initial begin
    @(posedge clk or negedge reset_n or negedge bus[3]);
    first_wake = $time;
    @(posedge clk, negedge reset_n, negedge bus[3]);
    second_wake = $time;
    @(posedge clk or negedge reset_n or negedge bus[3]);
    third_wake = $time;
  end

  initial begin
    @(posedge clk);
    one_waiter_at = $time;
  end

  initial begin
    @(posedge clk);
    other_waiter_at = $time;
  end

  initial begin
    #5;
    reset_n = 1'b0;
    #5;
    bus[3] = 1'b0;
    #5;
    clk = 1'b1;
  end

  final begin
    if (first_wake !== 5)
      $fatal(1, "first_wake was %0d, expected 5", first_wake);
    if (second_wake !== 10)
      $fatal(1, "second_wake was %0d, expected 10", second_wake);
    if (third_wake !== 15)
      $fatal(1, "third_wake was %0d, expected 15", third_wake);
    if (one_waiter_at !== 15)
      $fatal(1, "one_waiter_at was %0d, expected 15", one_waiter_at);
    if (other_waiter_at !== 15)
      $fatal(1, "other_waiter_at was %0d, expected 15", other_waiter_at);
    $display("All checks passed");
  end
endmodule
