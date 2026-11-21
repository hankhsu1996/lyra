// A value-change callback is called after each change of the object it is
// placed on (LRM 38.36.1) -- by a blocking write, a nonblocking update, or a
// write from foreign code -- and not by a write that leaves the object as it
// was, nor by a change of bits of the variable it is not placed on (LRM 9.4.2
// reads a change the same way). Each call is handed the object's value in the
// format the callback asked for and the time; on an element of an array, the
// element's index, an entry's key for an associative array; on a class
// variable, no value. Two changes in one time step are two calls. A callback
// may write another variable, which wakes what waits on it as any write does,
// and may remove itself or another callback (LRM 38.39), which is then not
// called again. Which of two callbacks on one object is called first is not
// fixed, so a callback removed by the other is called at most once. A write
// reaching the object some other way than naming it -- the whole array,
// structure or tagged union written, an element added beside it, another entry
// of its associative array written, a write through a `ref` argument bound to
// it or to an element beside it (LRM 13.5.2), a class variable assigned the
// object it already names, a member beside a class variable in a structure
// written, the variable a `ref` port is bound to assigned what it holds (LRM
// 23.3.3.2) -- calls it only where the object's value changed.
`timescale 1ns / 1ns

package items;
  class Item;
  endclass
endpackage

module Keeper
  import items::*;
(
    ref Item kept
);
endmodule

module Top;
  import items::*;

  import "DPI-C" context function int watch_all();
  import "DPI-C" context function void poke_i(int number);
  import "DPI-C" context function int calls_of(string name);
  import "DPI-C" context function int last_int_of(string name);
  import "DPI-C" context function real last_real_of(string name);
  import "DPI-C" context function int last_index_of(string name);
  import "DPI-C" context function int last_time_of(string name);
  import "DPI-C" context function int value_was_null(string name);
  import "DPI-C" context function int differences_so_far();

  typedef struct packed {
    bit [3:0] hi;
    bit [3:0] lo;
  } nibbles_t;

  typedef struct {
    int a;
    int b;
  } pair_t;

  typedef union tagged {
    int a;
    int b;
  } either_t;

  typedef union tagged {
    Item obj;
    int num;
  } maybe_t;

  int i = 0;
  logic [7:0] lv = 0;
  nibbles_t ps = 0;
  int ua[0:3];
  real r = 0.0;
  Item h;
  bit n = 0;
  int j = 0;
  int k = 0;
  pair_t us = '{a: 0, b: 0};
  int q[$] = '{0};
  either_t tu = tagged b 0;
  int va[int] = '{5: 0};
  int d[] = '{0, 0, 0, 0};

  typedef struct {
    Item held;
    int count;
  } holder_t;
  holder_t holders[2];
  Item handles[] = '{null, null};
  maybe_t mu = tagged obj null;
  Item kept_one;

  Keeper keeper (.kept(kept_one));

  task automatic name_through(ref Item target, input Item named);
    target = named;
  endtask

  task automatic set_through(ref int target, input int value);
    target = value;
  endtask
  int woke_n = 0;

  always @(n) woke_n++;

  initial begin
    if (watch_all() !== 0) $fatal(1, "placing the callbacks failed");
    #1;
    i = 1;
    i = 1;
    i = 2;
    i = 3;
    lv[5] = 1;
    lv[3] = 1;
    ps.lo = 4'h7;
    ps.hi = 4'h9;
    ua[1] = 6;
    ua[2] = 8;
    r = 2.5;
    h = new;
    j = 1;
    j = 2;
    k = 1;
    k = 2;
    ua = '{0, 6, 8, 3};
    h = h;
    us = '{a: 4, b: 0};
    us = '{a: 4, b: 7};
    q[$+1] = 1;
    q.push_back(2);
    q = '{0, 5};
    tu = tagged b 0;
    tu = tagged b 3;
    va[5] = 0;
    va[6] = 1;
    va[5] = 4;
    set_through(d[1], 7);
    set_through(d[1], 7);
    set_through(d[2], 9);
    holders[0].count = 3;
    holders[1].count = 4;
    holders = holders;
    holders[1].held = new;
    name_through(handles[1], handles[1]);
    name_through(handles[0], holders[1].held);
    name_through(handles[1], holders[1].held);
    mu = tagged obj null;
    mu = tagged obj h;
    kept_one = kept_one;
    kept_one = h;
    kept_one = h;
    #1 i <= 4;
    #1 poke_i(5);
    #1;
    if (calls_of("Top.i") !== 5)
      $fatal(1, "a callback on i was called %0d times, expected 5",
             calls_of("Top.i"));
    if (last_int_of("Top.i") !== 5)
      $fatal(1, "the last call on i was handed %0d", last_int_of("Top.i"));
    if (last_time_of("Top.i") !== 3)
      $fatal(1, "the last call on i was at %0d", last_time_of("Top.i"));
    if (woke_n !== 3)
      $fatal(1, "a wait on the notifier woke %0d times, expected 3", woke_n);
    // A scalar handed as vpi1 is 1 (LRM Annex K).
    if (calls_of("Top.lv[3]") !== 1 || last_int_of("Top.lv[3]") !== 1)
      $fatal(1, "a callback on lv[3] was called %0d times, handed %0d",
             calls_of("Top.lv[3]"), last_int_of("Top.lv[3]"));
    if (calls_of("Top.ps.hi") !== 1 || last_int_of("Top.ps.hi") !== 9)
      $fatal(1, "a callback on ps.hi was called %0d times, handed %0d",
             calls_of("Top.ps.hi"), last_int_of("Top.ps.hi"));
    if (calls_of("Top.ua[2]") !== 1 || last_int_of("Top.ua[2]") !== 8)
      $fatal(1, "a callback on ua[2] was called %0d times, handed %0d",
             calls_of("Top.ua[2]"), last_int_of("Top.ua[2]"));
    if (last_index_of("Top.ua[2]") !== 2)
      $fatal(1, "a call on ua[2] was handed the index %0d",
             last_index_of("Top.ua[2]"));
    if (calls_of("Top.r") !== 1 || last_real_of("Top.r") != 2.5)
      $fatal(1, "a callback on r was called %0d times, handed %f",
             calls_of("Top.r"), last_real_of("Top.r"));
    if (calls_of("Top.h") !== 1 || value_was_null("Top.h") !== 1)
      $fatal(1, "a callback on h was called %0d times, handed a value: %0d",
             calls_of("Top.h"), !value_was_null("Top.h"));
    if (calls_of("Top.j") !== 1)
      $fatal(1, "a callback that removed itself was called %0d times",
             calls_of("Top.j"));
    if (calls_of("Top.k first") !== 2)
      $fatal(1, "a callback on k was called %0d times, expected 2",
             calls_of("Top.k first"));
    if (calls_of("Top.k second") > 1)
      $fatal(1, "a removed callback on k was called %0d times",
             calls_of("Top.k second"));
    if (calls_of("Top.us.b") !== 1 || last_int_of("Top.us.b") !== 7)
      $fatal(1, "a callback on us.b was called %0d times, handed %0d",
             calls_of("Top.us.b"), last_int_of("Top.us.b"));
    if (calls_of("Top.q[0]") !== 0)
      $fatal(1, "a callback on q[0] was called %0d times, expected none",
             calls_of("Top.q[0]"));
    if (calls_of("Top.tu.b") !== 1 || last_int_of("Top.tu.b") !== 3)
      $fatal(1, "a callback on tu.b was called %0d times, handed %0d",
             calls_of("Top.tu.b"), last_int_of("Top.tu.b"));
    if (calls_of("Top.va[5]") !== 1 || last_int_of("Top.va[5]") !== 4)
      $fatal(1, "a callback on va[5] was called %0d times, handed %0d",
             calls_of("Top.va[5]"), last_int_of("Top.va[5]"));
    if (last_index_of("Top.va[5]") !== 5)
      $fatal(1, "a call on va[5] was handed the index %0d",
             last_index_of("Top.va[5]"));
    if (calls_of("Top.d[1]") !== 1 || last_int_of("Top.d[1]") !== 7)
      $fatal(1, "a callback on d[1] was called %0d times, handed %0d",
             calls_of("Top.d[1]"), last_int_of("Top.d[1]"));
    if (calls_of("Top.holders[1].held") !== 1)
      $fatal(1, "a callback on holders[1].held was called %0d times",
             calls_of("Top.holders[1].held"));
    if (calls_of("Top.handles[1]") !== 1)
      $fatal(1, "a callback on handles[1] was called %0d times",
             calls_of("Top.handles[1]"));
    if (calls_of("Top.mu.obj") !== 1)
      $fatal(1, "a callback on mu.obj was called %0d times",
             calls_of("Top.mu.obj"));
    if (calls_of("Top.keeper.kept") !== 1)
      $fatal(1, "a callback on keeper.kept was called %0d times",
             calls_of("Top.keeper.kept"));
    if (differences_so_far() !== 0)
      $fatal(1, "the calls differ in %0d answers", differences_so_far());
    $display("All checks passed");
  end
endmodule
