// A class property can be passed by reference (LRM 13.5.2), and the program
// is never left holding a reference to storage that has been reclaimed: an
// object stays while anything may still reach it (LRM 8.29). So a subroutine
// handed a property, or a part of one, goes on reading and writing it while
// the program lets go of the object -- across a wait in a task, inside a
// function that drops the last handle itself, through a `const ref`, through
// a reference handed on to another subroutine, into a mailbox's receive that
// waits for a message (LRM 15.4.9), and for an element of a dynamic array held
// in the object even when that array is deleted too.
module Top;
  typedef struct {
    int tag;
    int body;
  } rec_t;

  class Holder;
    int n;
    string s;
    int fixed [4];
    rec_t rec;
    int dynamic [];
  endclass

  Holder h;
  mailbox messages = new;
  int seen;
  string seen_text;

  task automatic hold(ref int value, ref string text);
    #2;
    seen = value;
    seen_text = text;
    value = 99;
  endtask

  task automatic hold_int(ref int value);
    #2;
    seen = value;
    value = 98;
  endtask

  task automatic hold_const(const ref int value);
    #2;
    seen = value;
  endtask

  task automatic hand_on(ref int value);
    hold_int(value);
  endtask

  task automatic wait_on(ref int value);
    @(value);
    seen = value;
  endtask

  function automatic int drop_then_read(ref int value);
    h = null;
    return value;
  endfunction

  function automatic Holder made();
    made = new;
    made.n = 64;
    made.s = "sixty-four characters of text, long enough to own its storage";
    made.fixed = '{1, 2, 30, 4};
    made.rec = '{tag: 7, body: 70};
    made.dynamic = '{10, 11, 12};
  endfunction

  initial begin
    Holder kept;

    h = made();
    fork hold(h.n, h.s); join_none
    #1 h = null;
    #2;
    if (seen !== 64) $fatal(1, "a property across a wait read %0d", seen);
    if (seen_text != "sixty-four characters of text, long enough to own its storage")
      $fatal(1, "a string property across a wait read '%s'", seen_text);

    h = made();
    seen = 0;
    seen = drop_then_read(h.n);
    if (seen !== 64)
      $fatal(1, "a function dropping the object read %0d", seen);

    h = made();
    seen = 0;
    fork hold_const(h.n); join_none
    #1 h = null;
    #2;
    if (seen !== 64) $fatal(1, "a const ref read %0d", seen);

    h = made();
    seen = 0;
    fork hold_int(h.fixed[2]); join_none
    #1 h = null;
    #2;
    if (seen !== 30) $fatal(1, "an element of a property read %0d", seen);

    h = made();
    seen = 0;
    fork hold_int(h.rec.body); join_none
    #1 h = null;
    #2;
    if (seen !== 70) $fatal(1, "a member of a property read %0d", seen);

    h = made();
    seen = 0;
    fork hold_int(h.dynamic[1]); join_none
    #1 h.dynamic.delete();
    h = null;
    #2;
    if (seen !== 11)
      $fatal(1, "an element of a deleted array in a property read %0d", seen);

    h = made();
    seen = 0;
    fork hand_on(h.n); join_none
    #1 h = null;
    #2;
    if (seen !== 64) $fatal(1, "a reference handed on read %0d", seen);

    h = made();
    kept = h;
    seen = 0;
    fork wait_on(h.n); join_none
    #1 h = null;
    kept.n = 5;
    kept = null;
    #1;
    if (seen !== 5) $fatal(1, "a wait on a property woke reading %0d", seen);

    h = made();
    fork messages.get(h.n); join_none
    #1 h = null;
    messages.put(77);
    #1;
    if (messages.num() !== 0)
      $fatal(1, "a receive into a dropped object's property left %0d messages",
             messages.num());

    $display("All checks passed");
  end
endmodule
