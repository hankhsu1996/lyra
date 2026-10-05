// An always_comb is sensitive to what any function it calls reads (LRM
// 9.2.2.2.1), and the clause analyzes a hierarchical function call and one
// from a package "as normal functions". So the function may be declared in
// another instance -- reached through an interface port, by a name climbing out
// of the reading instance, or down into a child -- or in a package, and may
// call another function in turn: a change to what it reads runs the block
// again. One module instantiated twice with its port bound to two different
// interface instances runs each copy on its own instance's change.
//
// What the block or a function it calls writes is left out of the list (LRM
// 9.2.2.2.1 b): a block writing a variable a called function reads does not
// run itself again by that write, even where the write is a nonblocking one
// that lands after the block has gone back to waiting. What is left out is
// only what is written: a function writing one element of an unpacked array
// leaves the block sensitive to the other elements it reads, and one writing
// some bits of a packed vector leaves it sensitive to the other bits it reads,
// down to the first bit past the write, either of which another process may
// write (LRM 9.2.2.2).
//
// A method called on a class object adds nothing to the list beyond its
// arguments (LRM 9.2.2.2.1), wherever the call is made -- in the block, or in
// a function the block calls -- so what the method reads does not run the
// block again.
package counters;
  int level = 0;

  function automatic int doubled_level();
    return 2 * level;
  endfunction

  function automatic int scaled_level();
    return doubled_level() + 1;
  endfunction
endpackage

interface Bus;
  int data = 0;
  int other = 0;

  function automatic int Doubled();
    return 2 * data;
  endfunction

  function automatic int Mix();
    return data + other;
  endfunction
endinterface

module ViaPort (Bus b);
  int seen = 0;
  int runs = 0;

  always_comb begin
    runs++;
    seen = b.Doubled();
  end
endmodule

module Climbing;
  int seen = 0;

  always_comb seen = Top.shared.Doubled();
endmodule

module Writer (Bus b);
  int seed = 0;
  int mixed = 0;
  int runs = 0;

  always_comb begin
    runs++;
    b.data <= seed;
    mixed = b.Mix();
  end
endmodule

module Top;
  Bus shared ();
  Bus first ();
  Bus second ();
  Bus own ();
  Bus written ();

  ViaPort on_first (first);
  ViaPort on_second (second);
  Climbing climbing ();
  Writer writer (written);

  int from_child = 0;
  int from_package = 0;
  int runs_before = 0;
  int cells[2];
  int other_cell = 0;

  function automatic void fill_first(int value);
    cells[0] = value;
  endfunction

  logic [7:0] lanes = 8'h00;
  logic [5:0] above_cleared = 6'h3f;

  function automatic void clear_low_lanes();
    lanes[3:0] = 4'h0;
  endfunction

  class Gauge;
    static int level = 0;
    function int peek();
      return level;
    endfunction
  endclass
  Gauge gauge = new();
  int through_object = 0;
  int object_runs = 0;
  int object_runs_before = 0;
  int in_block = 0;
  int in_block_runs = 0;
  int in_block_runs_before = 0;
  int second_runs_before = 0;
  int second_runs_after_first = 0;

  function automatic int peek_through_object();
    return gauge.peek();
  endfunction

  always_comb from_child = own.Doubled();
  always_comb from_package = counters::scaled_level();
  always_comb begin
    fill_first(1);
    other_cell = cells[1];
  end
  always_comb begin
    clear_low_lanes();
    above_cleared = lanes[7:2];
  end
  always_comb begin
    object_runs++;
    through_object = peek_through_object();
  end
  always_comb begin
    in_block_runs++;
    in_block = gauge.peek();
  end

  initial begin
    #1;
    runs_before = writer.runs;
    second_runs_before = on_second.runs;
    first.data = 3;
    #1;
    second_runs_after_first = on_second.runs;
    second.data = 5;
    shared.data = 7;
    own.data = 9;
    counters::level = 4;
    writer.seed = 6;
    cells[1] = 8;
    lanes[4] = 1'b1;
    object_runs_before = object_runs;
    in_block_runs_before = in_block_runs;
    Gauge::level = 3;
    #1;
    written.other = 1;
    #1;
  end

  final begin
    if (on_first.seen !== 6)
      $fatal(1, "a function reached through a port saw %0d, expected 6",
             on_first.seen);
    if (on_second.seen !== 10)
      $fatal(1, "the other copy's function saw %0d, expected 10",
             on_second.seen);
    if (second_runs_after_first !== second_runs_before)
      $fatal(1, "a change on one copy's instance ran the other copy %0d times",
             second_runs_after_first - second_runs_before);
    if (climbing.seen !== 14)
      $fatal(1, "a function reached by a climbing name saw %0d, expected 14",
             climbing.seen);
    if (from_child !== 18)
      $fatal(1, "a function of a child instance saw %0d, expected 18",
             from_child);
    if (from_package !== 9)
      $fatal(1, "a package function calling another saw %0d, expected 9",
             from_package);
    if (writer.mixed !== 7)
      $fatal(1, "a block writing what its function reads mixed %0d, expected 7",
             writer.mixed);
    if (object_runs !== object_runs_before || through_object !== 0)
      $fatal(1, "a method's read ran the block %0d more times and gave %0d, expected neither",
             object_runs - object_runs_before, through_object);
    if (in_block_runs !== in_block_runs_before || in_block !== 0)
      $fatal(1, "a method called in the block ran it %0d more times and gave %0d, expected neither",
             in_block_runs - in_block_runs_before, in_block);
    if (other_cell !== 8)
      $fatal(1, "an element no function writes saw %0d, expected 8",
             other_cell);
    if (above_cleared !== 6'd4)
      $fatal(1, "bits past what a function writes saw %0d, expected 4",
             above_cleared);
    if (writer.runs - runs_before !== 2)
      $fatal(1, "a block writing what its function reads ran %0d times, expected 2",
             writer.runs - runs_before);
    $display("All checks passed");
  end
endmodule
