// A random variable of an enumerated type the call chooses takes one of the
// values its enumeration names and no other value its base type holds (LRM
// 18.5): declared `rand`, as each element of an array declared random, listed
// to a call that names what it chooses (LRM 18.11), and handed to
// std::randomize() (LRM 18.12). A `randc` one runs through those values, each
// once before any repeats (LRM 18.4.2). A variable of the type that the call
// does not choose keeps whatever value it holds, named or not.
module Top;
  typedef enum bit [3:0] {LOW = 0, MID = 5, HIGH = 9} level_t;

  class Levels;
    rand level_t level;
    randc level_t cycled;
    rand level_t lanes[3];
    level_t held;
    constraint reads_held { held == 7 -> level != MID; }
  endclass

  int calls;
  int strays;
  int repeats;
  bit held_kept;
  int listed;
  int scoped;

  initial begin
    Levels l;
    level_t local_level;
    level_t seen[$];
    l = new;
    calls = -1;
    strays = -1;
    repeats = -1;
    listed = -1;
    scoped = -1;

    calls = 0;
    strays = 0;
    repeats = 0;
    l.held = level_t'(7);
    for (int n = 0; n < 60; n++) begin
      calls += l.randomize();
      if (!(l.level inside {LOW, HIGH})) strays++;
      if (!(l.cycled inside {LOW, MID, HIGH})) strays++;
      foreach (l.lanes[i]) if (!(l.lanes[i] inside {LOW, MID, HIGH})) strays++;
      seen.push_back(l.cycled);
      if (seen.size() == 3) begin
        if (seen[0] == seen[1] || seen[1] == seen[2] || seen[0] == seen[2])
          repeats++;
        seen.delete();
      end
    end
    held_kept = l.held == level_t'(7);

    listed = 0;
    for (int n = 0; n < 20; n++) begin
      l.held = level_t'(3);
      void'(l.randomize(held));
      if (l.held inside {LOW, MID, HIGH}) listed++;
    end

    scoped = 0;
    for (int n = 0; n < 20; n++) begin
      void'(std::randomize(local_level));
      if (local_level inside {LOW, MID, HIGH}) scoped++;
    end
  end

  final begin
    if (calls !== 60) $fatal(1, "%0d of 60 calls succeeded", calls);
    if (strays !== 0) $fatal(1, "%0d values named no member", strays);
    if (repeats !== 0) $fatal(1, "%0d cycles repeated a member", repeats);
    if (held_kept !== 1) $fatal(1, "a variable not chosen was changed");
    if (listed !== 20) $fatal(1, "a listed variable named no member %0d", listed);
    if (scoped !== 20) $fatal(1, "std::randomize named no member %0d", scoped);
    $display("All checks passed");
  end
endmodule
