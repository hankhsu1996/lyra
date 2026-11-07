// When a class object is created by a static declaration initializer there is
// no active thread, so the object's RNG is seeded with the next random value of
// the initialization RNG of the instance the declaration occurs in (LRM
// 18.14.1). What the initializers draw before the object is made is drawn from
// a generator of their own, not from that RNG, so it moves nothing the object
// is seeded with: two instances of one module, one of whose initializers draws
// first, make objects that randomize alike.
class Item;
  rand int unsigned value;
endclass

module Maker #(
    parameter bit DrawsFirst = 0
);
  int unsigned drawn = DrawsFirst ? $urandom : 0;
  Item made = new;
  int unsigned randomized;

  initial begin
    if (!made.randomize()) $fatal(1, "randomize failed");
    randomized = made.value;
  end
endmodule

module Top;
  Maker #(.DrawsFirst(0)) quiet ();
  Maker #(.DrawsFirst(1)) drawing ();

  final begin
    if (quiet.randomized !== drawing.randomized)
      $fatal(1, "the objects randomized to %0d and %0d", quiet.randomized,
             drawing.randomized);
    $display("All checks passed");
  end
endmodule
