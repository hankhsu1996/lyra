// An object's srandom seeds the generator its own randomize() draws from (LRM
// 18.13.3, 18.15), so seeding it again with the same value replays the values
// randomize() gave its random variables. And what it draws depends on that
// generator alone (LRM 18.14): another object randomized in between, of the
// same class and with the same constraints, changes none of it.
//
// The generator's state read by get_randstate and put back by set_randstate
// replays the same way (LRM 18.13.4, 18.13.5), and a pre_randomize seeding the
// object seeds the call that runs it, since the hook runs before the values are
// chosen (LRM 18.6.2). An object a random handle reaches is randomized with
// the one holding the handle (LRM 18.4), so seeding the holder replays both,
// and a child two holders share is randomized by each in turn.
module Top;
  class Packet;
    rand int unsigned payload;
  endclass

  class Window;
    rand bit [7:0] low;
    rand bit [7:0] high;
    constraint ordered { low < high; high - low < 40; }
  endclass

  class Seeded;
    rand bit [15:0] value;
    int seed;
    function void pre_randomize();
      this.srandom(seed);
    endfunction
  endclass

  class Leaf;
    rand bit [7:0] x;
    constraint bounded { x < 100; }
  endclass

  class Holder;
    rand Leaf leaf;
    rand bit [7:0] y;
    constraint above { y > leaf.x; }
    function new(Leaf shared);
      leaf = shared;
    endfunction
  endclass

  int unsigned first_payload, replayed_payload;
  bit [7:0] alone[8], interleaved[8];
  bit [7:0] window_low[4], restored_low[4];
  bit [15:0] seeded_first, seeded_again;
  bit [7:0] leaf_first, y_first, leaf_again, y_again;

  initial begin
    Packet packet;
    Window window, other;
    Seeded seeded;
    Leaf shared;
    Holder left, right;
    string state;

    packet = new;
    first_payload = 0;
    replayed_payload = 32'hFFFFFFFF;
    packet.srandom(42);
    if (packet.randomize() != 1) $fatal(1, "the first randomize() failed");
    first_payload = packet.payload;
    packet.srandom(42);
    if (packet.randomize() != 1) $fatal(1, "the second randomize() failed");
    replayed_payload = packet.payload;

    window = new;
    other = new;
    window.srandom(7);
    for (int i = 0; i < 8; i++) begin
      if (window.randomize() != 1) $fatal(1, "a window randomize() failed");
      alone[i] = window.low;
    end
    window.srandom(7);
    for (int i = 0; i < 8; i++) begin
      if (other.randomize() != 1) $fatal(1, "the other randomize() failed");
      if (window.randomize() != 1) $fatal(1, "a window randomize() failed");
      interleaved[i] = window.low;
    end

    state = window.get_randstate();
    for (int i = 0; i < 4; i++) begin
      if (window.randomize() != 1) $fatal(1, "a window randomize() failed");
      window_low[i] = window.low;
    end
    window.set_randstate(state);
    for (int i = 0; i < 4; i++) begin
      if (window.randomize() != 1) $fatal(1, "a window randomize() failed");
      restored_low[i] = window.low;
    end

    seeded = new;
    seeded.seed = 99;
    if (seeded.randomize() != 1) $fatal(1, "a seeded randomize() failed");
    seeded_first = seeded.value;
    if (seeded.randomize() != 1) $fatal(1, "a seeded randomize() failed");
    seeded_again = seeded.value;

    shared = new;
    left = new(shared);
    right = new(shared);
    left.srandom(5);
    if (left.randomize() != 1) $fatal(1, "the left holder's randomize() failed");
    if (!(left.y > shared.x && shared.x < 100))
      $fatal(1, "the left holder's values break its constraints");
    leaf_first = shared.x;
    y_first = left.y;
    if (right.randomize() != 1)
      $fatal(1, "the right holder's randomize() failed");
    if (!(right.y > shared.x && shared.x < 100))
      $fatal(1, "the right holder's values break its constraints");
    left.srandom(5);
    if (left.randomize() != 1) $fatal(1, "the left holder's randomize() failed");
    leaf_again = shared.x;
    y_again = left.y;
  end

  final begin
    if (first_payload !== replayed_payload)
      $fatal(1, "a reseeded object randomized %0h then %0h", first_payload,
             replayed_payload);
    for (int i = 0; i < 8; i++)
      if (alone[i] !== interleaved[i])
        $fatal(1, "draw %0d was %0d alone and %0d beside another object", i,
               alone[i], interleaved[i]);
    for (int i = 0; i < 4; i++)
      if (window_low[i] !== restored_low[i])
        $fatal(1, "draw %0d was %0d and %0d after its state was restored", i,
               window_low[i], restored_low[i]);
    if (seeded_first !== seeded_again)
      $fatal(1, "a pre_randomize seeding gave %0h then %0h", seeded_first,
             seeded_again);
    if (leaf_first !== leaf_again || y_first !== y_again)
      $fatal(1, "a reseeded holder gave x=%0d y=%0d then x=%0d y=%0d",
             leaf_first, y_first, leaf_again, y_again);
    $display("All checks passed");
  end
endmodule
