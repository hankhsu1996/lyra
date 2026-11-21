// A disable callback placed on a named block or a task is called after a
// `disable` ends it (LRM 38.36.1, 9.6.2), once for each disable: one naming
// it, and one naming a block or a task it is running inside, which ends it
// too -- a block nested in the one named, and a task down the chain of task
// enables from the one named. A disable of a block nothing is running inside
// ends nothing, and calls nothing back -- a block a disable just ended among
// them, though what ran in it has not left yet. One may be placed on a
// function too, which no `disable` can name and none from elsewhere can find
// running, since a function never waits (LRM 9.6.2), so it is not called. A
// callback removed by one called before it for the same disable is not called
// (LRM 38.39).
`timescale 1ns / 1ns

module Top;
  import "DPI-C" context function int watch_all();
  import "DPI-C" context function int calls_of(string name);
  import "DPI-C" context function int called_at(string name);
  import "DPI-C" context function int differences_so_far();

  int left_work = 0;

  task wait_long();
    #100;
  endtask

  task chain_tail();
    #100;
  endtask

  task chain_head();
    chain_tail();
  endtask

  function int twice(int n);
    return 2 * n;
  endfunction

  initial begin : work
    #50 left_work = 1;
  end

  initial wait_long();

  initial begin : outer
    begin : inner
      #100;
    end
  end

  initial chain_head();

  initial begin : done_early
  end

  initial begin : removed
    #100;
  end

  initial begin : twice_disabled
    #100;
  end

  // Checks that a disable called back once each where it ended something.
  function automatic void expect_one(string name, int at);
    if (calls_of(name) !== 1 || called_at(name) !== at)
      $fatal(1, "a disable of %s was called back %0d times, at %0d", name,
             calls_of(name), called_at(name));
  endfunction

  initial begin
    if (watch_all() !== 0) $fatal(1, "placing the callbacks failed");
    if (twice(2) !== 4) $fatal(1, "twice(2) is %0d", twice(2));
    #5 disable work;
    #2 disable wait_long;
    #2 disable outer;
    #2 disable chain_head;
    #2 disable done_early;
    #2 disable removed;
    #2 begin
      disable twice_disabled;
      disable twice_disabled;
    end
    #10;
    if (left_work !== 0) $fatal(1, "the disabled block went on");
    expect_one("Top.work", 5);
    expect_one("Top.wait_long", 7);
    expect_one("Top.outer", 9);
    expect_one("Top.outer.inner", 9);
    expect_one("Top.chain_head", 11);
    expect_one("Top.chain_tail", 11);
    expect_one("Top.removed, removing", 15);
    expect_one("Top.twice_disabled", 17);
    if (calls_of("Top.removed, removed") !== 0)
      $fatal(1, "a callback removed before its call was called");
    if (calls_of("Top.done_early") !== 0)
      $fatal(1, "a disable of a block nothing ran inside was called back");
    if (calls_of("Top.twice") !== 0)
      $fatal(1, "a callback on a function was called");
    if (differences_so_far() !== 0)
      $fatal(1, "the calls differ in %0d answers", differences_so_far());
    $display("All checks passed");
  end
endmodule
