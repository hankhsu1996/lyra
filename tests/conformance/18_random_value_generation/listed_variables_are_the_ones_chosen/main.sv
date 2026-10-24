// std::randomize() chooses the variables it lists so that its inline
// constraints hold (LRM 18.12), and a class's randomize() given a list chooses
// exactly the variables listed, whether or not they were declared random,
// holding every other one while still checking every constraint (LRM 18.11).
// A listed array is each of its elements, and its size where a constraint
// names it, as though its mode were on; a random handle the call does not list
// is a state variable, so the object it reaches is neither randomized nor
// asked to prepare.
module Top;
  int inner_prepared;

  class Inner;
    rand bit [7:0] v;
    constraint above { v > 3; }
    function void pre_randomize();
      inner_prepared++;
    endfunction
  endclass

  class Pair;
    rand bit [7:0] chosen;
    rand bit [7:0] held;
    bit [7:0] state;
    bit [7:0] lanes[3];
    bit [7:0] tail[$];
    rand Inner inner;
    constraint order { chosen < held; }
    function new;
      inner = new;
    endfunction
  endclass

  int scope_result;
  bit scope_held;
  int listed_result;
  bit listed_kept;
  bit state_chosen;
  int array_result;
  bit array_held;
  int queue_result;
  bit queue_held;
  bit inner_kept;

  initial begin
    Pair p;
    int a;
    int b;
    p = new;
    scope_result = -1;
    scope_held = 0;
    listed_result = -1;
    listed_kept = 0;
    state_chosen = 0;
    array_result = -1;
    array_held = 0;
    queue_result = -1;
    queue_held = 0;
    inner_kept = 0;

    scope_result = std::randomize(a, b) with { a < b; b < 100; a > 0; };
    scope_held = a < b && b < 100 && a > 0;

    p.held = 50;
    listed_result = p.randomize(chosen);
    listed_kept = p.held == 50 && p.chosen < 50;

    p.state = 0;
    void'(p.randomize(state) with { state == 123; });
    state_chosen = p.state == 123;

    inner_prepared = 0;
    p.inner.v = 0;
    array_result = p.randomize(lanes) with {
      foreach (lanes[i]) lanes[i] > 100;
    };
    array_held = p.lanes[0] > 100 && p.lanes[1] > 100 && p.lanes[2] > 100;
    p.tail = {1, 2};
    queue_result = p.randomize(tail) with { tail.size() == 3; tail[2] == 9; };
    queue_held = p.tail.size() == 3 && p.tail[2] == 9;
    inner_kept = p.inner.v == 0 && inner_prepared == 0;
  end

  final begin
    if (scope_result !== 1) $fatal(1, "std::randomize answered %0d", scope_result);
    if (scope_held !== 1) $fatal(1, "std::randomize broke its constraints");
    if (listed_result !== 1) $fatal(1, "a listing call answered %0d", listed_result);
    if (listed_kept !== 1)
      $fatal(1, "a listing call chose a variable it did not list");
    if (state_chosen !== 1)
      $fatal(1, "a listed variable not declared random was not chosen");
    if (array_result !== 1 || array_held !== 1)
      $fatal(1, "a listed array answered %0d", array_result);
    if (queue_result !== 1 || queue_held !== 1)
      $fatal(1, "a listed queue answered %0d", queue_result);
    if (inner_kept !== 1)
      $fatal(1, "an object no listed handle reaches was randomized");
    $display("All checks passed");
  end
endmodule
