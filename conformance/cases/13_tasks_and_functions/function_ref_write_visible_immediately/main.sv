// Because a ref argument shares the caller's variable rather than a copy of
// it, a write through the formal is visible outside the subroutine as soon as
// it happens and not at the return: the variable read by its own name from
// inside the body already carries the new value, and the write is a change
// that an event control on that variable observes (LRM 13.5.2, 9.4.2).
//
// A member of an unpacked structure and an element of an unpacked array are
// passed by reference the same way (LRM 13.5.2), and a write through one is a
// change of the variable it is part of (LRM 4.3). So a task that writes through
// such a formal and then waits wakes a process waiting on that part at the
// write, not when the task returns. Binding a formal to an associative entry
// that does not exist yet creates it (LRM 7.8.7), and that too is a change of
// the array at the call, whatever the task goes on to do.
//
// A class property, and an element of one, is passed by reference the same way
// (LRM 13.5.2), and a write through it is a change of the object that a wait
// reaching the property observes (LRM 9.4.2) -- at the write as well.
class Holder;
  int v;
  int parts[2];
endclass

module Top;
  typedef struct {
    int a;
    int b;
  } pair_t;

  int g;
  int seen_inside;
  int woke;

  int arr[3];
  pair_t s;
  pair_t nest[2];
  int aa[string];

  time arr_woke_at = -1;
  time s_woke_at = -1;
  time nest_woke_at = -1;
  time aa_woke_at = -1;
  time property_woke_at = -1;
  time property_part_woke_at = -1;
  int arr_seen_inside;
  Holder o = new;

  function automatic void poke(ref int x);
    x = x + 1;
    seen_inside = g;
  endfunction

  task automatic poke_then_wait(ref int x);
    x = x + 1;
    arr_seen_inside = arr[1];
    #5;
  endtask

  task automatic hold(ref int x);
    #5;
  endtask

  initial begin
    @(g);
    woke = 1;
  end

  initial begin
    @(arr[1]);
    arr_woke_at = $time;
  end

  initial begin
    @(s.b);
    s_woke_at = $time;
  end

  initial begin
    @(nest[1].b);
    nest_woke_at = $time;
  end

  initial begin
    wait (aa.num() == 1);
    aa_woke_at = $time;
  end

  initial begin
    @(o.v);
    property_woke_at = $time;
  end

  initial begin
    @(o.parts[1]);
    property_part_woke_at = $time;
  end

  initial begin
    #1;
    poke(g);
    fork
      poke_then_wait(arr[1]);
      poke_then_wait(s.b);
      poke_then_wait(nest[1].b);
      hold(aa["k"]);
      poke_then_wait(o.v);
      poke_then_wait(o.parts[1]);
    join
  end

  final begin
    if (g !== 1) $fatal(1, "g was %0d, expected 1", g);
    if (seen_inside !== 1)
      $fatal(1, "seen_inside was %0d, expected 1", seen_inside);
    if (woke !== 1) $fatal(1, "woke was %0d, expected 1", woke);

    if (arr[1] !== 1) $fatal(1, "arr[1] was %0d, expected 1", arr[1]);
    if (arr_seen_inside !== 1)
      $fatal(1, "arr_seen_inside was %0d, expected 1", arr_seen_inside);
    if (arr_woke_at !== 1)
      $fatal(1, "a wait on arr[1] woke at %0t, expected 1", arr_woke_at);
    if (s.b !== 1) $fatal(1, "s.b was %0d, expected 1", s.b);
    if (s_woke_at !== 1)
      $fatal(1, "a wait on s.b woke at %0t, expected 1", s_woke_at);
    if (nest[1].b !== 1)
      $fatal(1, "nest[1].b was %0d, expected 1", nest[1].b);
    if (nest_woke_at !== 1)
      $fatal(1, "a wait on nest[1].b woke at %0t, expected 1", nest_woke_at);
    if (!aa.exists("k")) $fatal(1, "aa[\"k\"] was never created");
    if (aa_woke_at !== 1)
      $fatal(1, "a wait on aa.num() woke at %0t, expected 1", aa_woke_at);
    if (o.v !== 1) $fatal(1, "o.v was %0d, expected 1", o.v);
    if (property_woke_at !== 1)
      $fatal(1, "a wait on o.v woke at %0t, expected 1", property_woke_at);
    if (o.parts[1] !== 1)
      $fatal(1, "o.parts[1] was %0d, expected 1", o.parts[1]);
    if (property_part_woke_at !== 1)
      $fatal(1, "a wait on o.parts[1] woke at %0t, expected 1",
             property_part_woke_at);
    $display("All checks passed");
  end
endmodule
