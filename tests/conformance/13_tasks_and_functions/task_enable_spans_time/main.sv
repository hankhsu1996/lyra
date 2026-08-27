// A task may contain time-controlling statements, and control is passed back
// to the enabling process only after the task has completed, so the time at
// which a task is enabled can differ from the time at which control returns.
// That holds however the task consumes time -- a delay control or an event
// control -- and it holds through a chain of enables, because control does
// not return until every task enabled has completed
// (LRM 13.2, 13.3, 9.4.1, 9.4.2).
module Top;
  timeunit 1ns;
  timeprecision 1ps;

  int delayed;
  int after_delay_time;
  int mid_delayed;

  int go;
  int woke;
  int after_event_time;

  int chained;
  int after_chain_time;
  int mid_chained;

  task automatic set_after_delay(input int v);
    #5;
    delayed = v;
  endtask

  task automatic wait_for_go();
    @(go);
    woke = 1;
  endtask

  task automatic inner();
    #5;
    chained = chained + 1;
  endtask

  task automatic outer();
    inner();
    chained = chained + 10;
  endtask

  initial begin
    delayed = 3;
    set_after_delay(7);
    after_delay_time = $time;
  end

  initial begin
    #2;
    mid_delayed = delayed;
  end

  initial begin
    go = 0;
    wait_for_go();
    after_event_time = $time;
  end

  initial begin
    #3;
    go = 1;
  end

  initial begin
    chained = 100;
    outer();
    after_chain_time = $time;
  end

  initial begin
    #2;
    mid_chained = chained;
  end

  final begin
    if (mid_delayed !== 3)
      $fatal(1, "mid_delayed was %0d, expected 3", mid_delayed);
    if (delayed !== 7) $fatal(1, "delayed was %0d, expected 7", delayed);
    if (after_delay_time !== 5)
      $fatal(1, "after_delay_time was %0d, expected 5", after_delay_time);
    if (woke !== 1) $fatal(1, "woke was %0d, expected 1", woke);
    if (after_event_time !== 3)
      $fatal(1, "after_event_time was %0d, expected 3", after_event_time);
    if (mid_chained !== 100)
      $fatal(1, "mid_chained was %0d, expected 100", mid_chained);
    if (chained !== 111) $fatal(1, "chained was %0d, expected 111", chained);
    if (after_chain_time !== 5)
      $fatal(1, "after_chain_time was %0d, expected 5", after_chain_time);
    $display("All checks passed");
  end
endmodule
