// A dynamic array type is structural (LRM 6.22.1 f), as a fixed-size array of
// them is, so each module does with the type only what it does. Keeper holds a
// dynamic array and writes an element another module waits on (LRM 9.4.2).
// Nester builds a fixed-size array of dynamic arrays and compares nothing. Top
// copies that array, compares the copy with the one Nester built element by
// element (LRM 7.4.6, 11.4.5), compares and sorts what Keeper holds (LRM
// 7.12.2), and streams it (LRM 11.4.14), so every operation of the type is
// asked by a module other than the one naming the type first. Keeper also
// holds a dynamic array of an enumeration whose base type is that of the
// first's elements, a different type (LRM 6.19, 6.22.1), and dynamic arrays
// of handles of two classes, two types (LRM 8.4); and Top writes a dynamic
// array over itself, which keeps its elements (LRM 7.6). A package declares a
// class and holds a dynamic array of its handles, which Top copies, so the
// type is one whether the unit naming it declares the class or imports it
// (LRM 26.3). A queue type is structural too (LRM 6.22.1 f): Keeper holds a
// bounded queue and writes an element another module waits on, Nester builds
// a fixed-size array of queues, and Top copies, compares, sorts, streams and
// pops them, a bounded queue of its own keeping what its bound admits of a longer one
// (LRM 7.10.5); the package holds a queue of its class's handles.
typedef int dyn_t [];
typedef dyn_t rows_t [2];
typedef int lane_t [$:3];
typedef lane_t lanes_t [2];
typedef enum int {kLow, kMid, kHigh} level_t;

class Cat;
  int lives = 9;
endclass

class Dog;
  string name = "rex";
endclass

package Aviary;
  class Bird;
    int wings = 2;
  endclass

  typedef Bird flock_t [];
  flock_t flock;
  Bird perches [$];

  function automatic void Hatch();
    flock = new[2];
    flock[1] = new;
    perches.push_back(flock[1]);
  endfunction
endpackage

module Keeper;
  dyn_t held;
  level_t levels [];
  Cat cats [];
  Dog dogs [];
  lane_t lane;

  initial begin
    lane = '{40, 20, 30};
    levels = new[2];
    levels[1] = kHigh;
    cats = new[1];
    cats[0] = new;
    dogs = new[2];
    dogs[1] = new;
    held = new[3];
    held[0] = 30;
    held[1] = 10;
    held[2] = 20;
    #2;
    held[1] = 7;
    lane[2] = 9;
  end
endmodule

module Nester;
  rows_t rows;
  lanes_t lanes;

  initial begin
    rows[0] = new[2];
    rows[0][1] = 5;
    rows[1] = new[1];
    lanes[0].push_back(4);
    lanes[0].push_back(5);
    lanes[1].push_front(6);
  end
endmodule

module Top;
  Keeper keeper ();
  Nester nester ();

  rows_t copied;
  Aviary::flock_t birds;
  dyn_t sorted;
  dyn_t expected;
  bit [95:0] streamed;
  lanes_t lanes_copied;
  lane_t lane_sorted;
  int lane_free [$];

  task automatic CheckLanes();
    lanes_copied = nester.lanes;
    if (lanes_copied != nester.lanes) $fatal(1, "a copy of the lanes differed");
    lanes_copied[0][1] = 8;
    if (lanes_copied == nester.lanes) $fatal(1, "lanes of different elements compared equal");
    if (nester.lanes[0][1] != 5) $fatal(1, "writing the copy wrote the lanes it was taken from");

    lane_sorted = keeper.lane;
    lane_sorted.sort();
    if (lane_sorted != lane_t'{20, 30, 40}) $fatal(1, "the sorted lane held %p", lane_sorted);
    if (keeper.lane[0] != 40) $fatal(1, "sorting a copy reordered the held lane");
    streamed = {>>{keeper.lane}};
    if (streamed != {32'd40, 32'd20, 32'd30}) $fatal(1, "the lane streamed %h", streamed);
    lane_free = '{1, 2, 3, 4, 5};
    lane_sorted = lane_free;
    if (lane_sorted.size() != 4 || lane_sorted[3] != 4)
      $fatal(1, "a bounded copy held %p", lane_sorted);
    if (lane_sorted.pop_front() != 1 || lane_sorted.size() != 3)
      $fatal(1, "a pop left %p", lane_sorted);
  endtask

  initial begin
    #1;
    copied = nester.rows;
    if (copied != nester.rows) $fatal(1, "a copy of the rows differed");
    if (!(copied === nester.rows)) $fatal(1, "a copy of the rows was not identical");
    copied[0][1] = 6;
    if (copied == nester.rows) $fatal(1, "rows of different elements compared equal");
    if (nester.rows[0][1] != 5) $fatal(1, "writing the copy wrote the rows it was taken from");

    expected = '{30, 10, 20};
    if (keeper.held != expected) $fatal(1, "the held array differed");
    sorted = keeper.held;
    sorted.sort();
    if (sorted[0] != 10 || sorted[2] != 30) $fatal(1, "the sorted copy held %p", sorted);
    if (keeper.held[0] != 30) $fatal(1, "sorting a copy reordered the held array");
    streamed = {>>{keeper.held}};
    if (streamed != {32'd30, 32'd10, 32'd20}) $fatal(1, "the held array streamed %h", streamed);
    if (keeper.levels.size() != 2 || keeper.levels[1] != kHigh || keeper.levels[0] != kLow)
      $fatal(1, "the levels held %p", keeper.levels);
    if (keeper.cats.size() != 1 || keeper.cats[0].lives != 9)
      $fatal(1, "the cats held %0d", keeper.cats.size());
    if (keeper.dogs.size() != 2 || keeper.dogs[0] != null || keeper.dogs[1].name != "rex")
      $fatal(1, "the dogs held %0d", keeper.dogs.size());

    Aviary::Hatch();
    birds = Aviary::flock;
    if (birds.size() != 2 || birds[0] != null || birds[1].wings != 2)
      $fatal(1, "the copied flock held %0d", birds.size());
    birds[1].wings = 3;
    if (Aviary::flock[1].wings != 3) $fatal(1, "a copied handle reached another bird");
    if (Aviary::perches.size() != 1 || Aviary::perches[0].wings != 3)
      $fatal(1, "the perches held %0d", Aviary::perches.size());
    CheckLanes();

    sorted = sorted;
    if (sorted.size() != 3 || sorted[0] != 10 || sorted[1] != 20 || sorted[2] != 30)
      $fatal(1, "an array written over itself held %p", sorted);
    sorted[0] = 11;
    if (sorted[0] != 11 || sorted[1] != 20) $fatal(1, "a write after writing over itself held %p", sorted);

    fork
      begin
        @(keeper.held[1]);
        if (keeper.held[1] != 7) $fatal(1, "the watched element held %0d", keeper.held[1]);
      end
      begin
        @(keeper.lane[2]);
        if (keeper.lane[2] != 9) $fatal(1, "the watched lane element held %0d", keeper.lane[2]);
      end
    join
    $display("All checks passed");
  end
endmodule
