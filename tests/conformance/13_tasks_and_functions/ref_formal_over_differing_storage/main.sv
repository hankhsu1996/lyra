// One subroutine serves every kind of storage the standard permits as a
// reference actual. A variable, a class property and an automatic variable are
// three of the four categories a ref argument may be passed (LRM 13.5.2), and
// the subroutine states none of them: it declares one formal and is written
// once, so which kind a call hands it is the caller's alone.
//
// They are not alike in what the language says about a write to them. A write
// to a variable of the design hierarchy is a value change a process waiting on
// that variable observes (LRM 9.4.2); a class property and an automatic
// variable carry no such obligation. So the same statement in the same body
// has to raise an event for one actual and not for the others, and what it
// writes has to reach the caller's own storage in every case, immediately and
// before the subroutine returns.
//
// The fourth category, a member of an unpacked structure or an element of an
// unpacked array, is a part of one of the other three, and it is passed
// whichever of them it is part of: a design variable, where the write is a
// change of that variable, an automatic variable, a class property, or the
// storage another ref formal already holds.
module Top;
  typedef struct {
    int a;
    int b;
  } pair_t;

  class Holder;
    int owned;
    int slots[3];
  endclass

  Holder held;
  int scoped;
  int carried;
  int wakes;

  int scoped_parts[3];
  int part_wakes;
  int carried_element;
  int carried_member;

  task automatic add_to(ref int slot, input int amount);
    slot = slot + amount;
  endtask

  task automatic add_to_member(ref pair_t pair, input int amount);
    add_to(pair.b, amount);
  endtask

  always @(scoped) wakes = wakes + 1;
  always @(scoped_parts[2]) part_wakes = part_wakes + 1;

  initial begin
    automatic int detached = 300;
    automatic int detached_parts[3] = '{0, 400, 0};
    automatic pair_t detached_pair = '{a: 0, b: 500};

    held = new();
    held.owned = 200;
    held.slots[2] = 600;
    scoped = 0;
    carried = -1;
    #1;
    wakes = 0;
    part_wakes = 0;

    add_to(scoped, 7);
    add_to(held.owned, 7);
    add_to(detached, 7);
    carried = detached;

    add_to(scoped_parts[2], 7);
    add_to(held.slots[2], 7);
    add_to(detached_parts[1], 7);
    add_to_member(detached_pair, 7);
    carried_element = detached_parts[1];
    carried_member = detached_pair.b;
    #1;
  end

  final begin
    if (scoped !== 7) $fatal(1, "scoped was %0d, expected 7", scoped);
    if (held.owned !== 207)
      $fatal(1, "held.owned was %0d, expected 207", held.owned);
    if (carried !== 307) $fatal(1, "carried was %0d, expected 307", carried);
    if (wakes !== 1) $fatal(1, "wakes was %0d, expected 1", wakes);

    if (scoped_parts[2] !== 7)
      $fatal(1, "scoped_parts[2] was %0d, expected 7", scoped_parts[2]);
    if (part_wakes !== 1)
      $fatal(1, "part_wakes was %0d, expected 1", part_wakes);
    if (held.slots[2] !== 607)
      $fatal(1, "held.slots[2] was %0d, expected 607", held.slots[2]);
    if (carried_element !== 407)
      $fatal(1, "carried_element was %0d, expected 407", carried_element);
    if (carried_member !== 507)
      $fatal(1, "carried_member was %0d, expected 507", carried_member);
    $display("All checks passed");
  end
endmodule
