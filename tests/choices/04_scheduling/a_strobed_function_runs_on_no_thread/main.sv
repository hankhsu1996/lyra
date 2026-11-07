// @top: Top
//
// A strobed write's arguments are evaluated at the end of the time step, after
// every process of it has run (LRM 21.2.2), and the standard names no thread
// that evaluates them. Lyra evaluates them on no thread, as a static
// initializer's code runs (LRM 18.14.1 "there is no active thread"):
//
// - `process::self()` answers null there (LRM 9.7), `disable fork` ends
//   nothing, since no calling process has descendants (LRM 9.6.3), `kill()`
//   ends the process it names and leaves the caller running, and a covergroup
//   sampled on its event made there starts waiting for it at once (LRM 19.3).
// - A draw there comes from a generator the process that called the task
//   handed over, taken from its state and the count of what it has handed
//   over since it was last seeded, without stepping it. So the process's own
//   draws are the same however many strobes it makes (LRM 18.14), the value a
//   strobe draws does not depend on the order processes ran in (LRM 18.14.2),
//   and processes seeded alike hand over alike whatever they handed over
//   before (LRM 18.13.3, 18.14.1).
// - A strobed write reached from a static initializer, or from another
//   strobed write's argument, draws the same way from what reached it.
//
// Each instance of `Draws` and `Pair` starts from the same seeds as every
// other (LRM 18.14.1), so the comparisons below hold between instances.
module Draws #(
    parameter int Id = 0,
    parameter int ExtraStrobes = 0,
    parameter bit Seeded = 0
);
  int fd;
  int unsigned first_draw;
  int unsigned last_draw;
  string strobed;

  initial begin
    fd = $fopen($sformatf("no_thread_draws_%0d.txt", Id), "w");
    repeat (ExtraStrobes) $fstrobe(fd, "extra");
    if (Seeded) begin
      process p;
      p = process::self();
      p.srandom(7);
    end
    first_draw = $urandom;
    $fstrobe(fd, "%0d", $urandom);
    last_draw = $urandom;
    #1 $fclose(fd);

    fd = $fopen($sformatf("no_thread_draws_%0d.txt", Id), "r");
    repeat (ExtraStrobes + 1) void'($fgets(strobed, fd));
    $fclose(fd);
  end
endmodule

// A process a strobed function ends before it carries on.
module Victim;
  process handle;
  bit carried_on = 0;

  initial begin
    handle = process::self();
    #5 carried_on = 1;
  end
endmodule

// Two processes each strobe a draw; where `Late` holds, the first lets the
// second strobe before it does.
module Pair #(
    parameter int Id = 0,
    parameter bit Late = 0
);
  int first_fd;
  int second_fd;
  string first;
  string second;

  initial begin
    first_fd = $fopen($sformatf("no_thread_pair_%0d_first.txt", Id), "w");
    if (Late) #0;
    $fstrobe(first_fd, "%0d", $urandom);
    #1 $fclose(first_fd);
    first_fd = $fopen($sformatf("no_thread_pair_%0d_first.txt", Id), "r");
    void'($fgets(first, first_fd));
    $fclose(first_fd);
  end

  initial begin
    second_fd = $fopen($sformatf("no_thread_pair_%0d_second.txt", Id), "w");
    $fstrobe(second_fd, "%0d", $urandom);
    #1 $fclose(second_fd);
    second_fd = $fopen($sformatf("no_thread_pair_%0d_second.txt", Id), "r");
    void'($fgets(second, second_fd));
    $fclose(second_fd);
  end
endmodule

module Top;
  int fd;
  int inner_fd;
  string answers;
  string inner;

  function automatic int no_process();
    process p;
    p = process::self();
    return p == null;
  endfunction

  function automatic int no_descendants();
    disable fork;
    return 1;
  endfunction

  bit clk;

  covergroup Sampled @(posedge clk);
    coverpoint clk;
  endgroup

  function automatic int samples_on_its_event();
    Sampled sampled = new;
    return 1;
  endfunction

  function automatic int stops_the_victim();
    victim.handle.kill();
    return 1;
  endfunction

  function automatic int strobes_again();
    $fstrobe(inner_fd, "%0d", $urandom_range(4, 4));
    return 1;
  endfunction

  int init_fd;

  function automatic int strobe_from_an_initializer();
    init_fd = $fopen("no_thread_from_an_initializer.txt", "w");
    $fstrobe(init_fd, "%0d", $urandom_range(5, 5));
    return 1;
  endfunction

  static int initialized = strobe_from_an_initializer();

  Draws #(.Id(0)) plain ();
  Draws #(.Id(1), .ExtraStrobes(3)) strobing ();
  Draws #(.Id(2), .Seeded(1)) seeded ();
  Draws #(.Id(3), .ExtraStrobes(2), .Seeded(1)) seeded_again ();
  Victim victim ();
  Pair #(.Id(0)) in_order ();
  Pair #(.Id(1), .Late(1)) out_of_order ();

  string from_initializer;

  initial begin
    fd = $fopen("no_thread_answers.txt", "w");
    inner_fd = $fopen("no_thread_inner.txt", "w");
    $fstrobe(fd, "%0d %0d %0d %0d %0d", no_process(), no_descendants(),
             stops_the_victim(), samples_on_its_event(), strobes_again());
    #1 $fclose(fd);
    $fclose(inner_fd);
    $fclose(init_fd);

    fd = $fopen("no_thread_answers.txt", "r");
    void'($fgets(answers, fd));
    $fclose(fd);
    fd = $fopen("no_thread_inner.txt", "r");
    void'($fgets(inner, fd));
    $fclose(fd);
    fd = $fopen("no_thread_from_an_initializer.txt", "r");
    void'($fgets(from_initializer, fd));
    $fclose(fd);
  end

  final begin
    if (answers != "1 1 1 1 1\n")
      $fatal(1, "no process, no descendants, a kill, a covergroup and a nested strobe gave '%s'",
             answers);
    if (victim.carried_on !== 0)
      $fatal(1, "the process a strobed function killed carried on");
    if (inner != "4\n")
      $fatal(1, "the strobe a strobed argument made wrote '%s'", inner);
    if (from_initializer != "5\n")
      $fatal(1, "the strobe a static initializer made wrote '%s'",
             from_initializer);

    if (plain.first_draw !== strobing.first_draw)
      $fatal(1, "two instances drew %0d and %0d first", plain.first_draw,
             strobing.first_draw);
    if (plain.last_draw !== strobing.last_draw)
      $fatal(1, "three more strobes moved the process's draw from %0d to %0d",
             plain.last_draw, strobing.last_draw);
    if (seeded.strobed != seeded_again.strobed)
      $fatal(1, "two processes seeded alike strobed '%s' and '%s'",
             seeded.strobed, seeded_again.strobed);
    if (seeded.strobed == plain.strobed)
      $fatal(1, "a process seeded anew strobed what an unseeded one did");

    if (in_order.first != out_of_order.first)
      $fatal(1, "the first process strobed '%s', and '%s' once it strobed last",
             in_order.first, out_of_order.first);
    if (in_order.second != out_of_order.second)
      $fatal(1, "the second process strobed '%s', and '%s' once it strobed first",
             in_order.second, out_of_order.second);
    if (in_order.first == in_order.second)
      $fatal(1, "two processes strobed the same draw '%s'", in_order.first);
    $display("All checks passed");
  end
endmodule
