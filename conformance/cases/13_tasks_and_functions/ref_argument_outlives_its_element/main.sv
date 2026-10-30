// An element of a dynamic array, a queue or an associative array passed by
// reference goes on existing until the called subroutines complete, even when
// the array removes it or is resized first; the reference is then outdated,
// and what is written through it is not seen in the array (LRM 13.5.2). Which
// operations outdate a reference is fixed: deleting or resizing a dynamic
// array, assigning a whole queue or deleting the element from it, and deleting
// the entry from an associative array (LRM 13.5.2, 7.10.3). Inserting into a
// queue outdates nothing, so the reference goes on naming its element at the
// position the element has moved to. An element put at the outdated element's
// index afterwards is a new one, which the reference does not reach. The
// element keeps its value whatever its type, a string or a structure holding
// one included, and two subroutines holding the same element each keep it
// until the second completes. A process ended while it holds one -- killed,
// or its block disabled -- lets it go.
module Top;
  typedef struct {
    int tag;
    string text;
  } tagged_text_t;

  int dynamic [];
  int queue [$];
  int assoc [int];
  string texts [$];
  tagged_text_t records [$];
  tagged_text_t seen_record;
  int seen;
  int seen_second;
  string seen_text;
  process held;

  task automatic hold(ref int element);
    #2;
    seen = element;
    element = 99;
  endtask

  task automatic hold_longer(ref int element);
    #4;
    seen_second = element;
    element = 98;
  endtask

  task automatic hold_text(ref string text);
    #2;
    seen_text = text;
  endtask

  task automatic hold_record(ref tagged_text_t record);
    #2;
    seen_record = record;
  endtask

  task automatic hold_forever(ref int element);
    #1000;
    seen = element;
  endtask

  initial begin
    dynamic = '{10, 11, 12, 13};
    seen = 0;
    fork hold(dynamic[2]); join_none
    #1 dynamic.delete();
    #2;
    if (seen !== 12) $fatal(1, "an element of a deleted dynamic array read %0d", seen);
    if (dynamic.size() !== 0)
      $fatal(1, "a write through an outdated reference grew the array to %0d", dynamic.size());

    dynamic = '{10, 11, 42, 13};
    seen = 0;
    fork hold(dynamic[2]); join_none
    #1 dynamic = new[8](dynamic);
    #2;
    if (seen !== 42) $fatal(1, "an element of a resized dynamic array read %0d", seen);
    if (dynamic[2] !== 42)
      $fatal(1, "a write through an outdated reference reached the resized array: %0d",
             dynamic[2]);

    queue = '{20, 21, 22};
    seen = 0;
    fork hold(queue[1]); join_none
    #1 queue.delete(1);
    queue.insert(1, 50);
    #2;
    if (seen !== 21) $fatal(1, "a deleted queue element read %0d", seen);
    if (queue[1] !== 50)
      $fatal(1, "the element put at a deleted one's index became %0d", queue[1]);

    queue = '{20, 21, 22};
    seen = 0;
    fork hold(queue[1]); join_none
    #1 queue.push_front(10);
    queue.push_front(11);
    queue.insert(1, 12);
    #2;
    if (seen !== 21) $fatal(1, "an element moved by insertions read %0d", seen);
    if (queue[4] !== 99)
      $fatal(1, "a write through a moved element left %0d where it moved", queue[4]);

    queue = '{20, 21, 22};
    seen = 0;
    fork hold(queue[2]); join_none
    #1 void'(queue.pop_back());
    #2;
    if (seen !== 22) $fatal(1, "a popped queue element read %0d", seen);
    if (queue.size() !== 2)
      $fatal(1, "a write through a popped element grew the queue to %0d", queue.size());

    queue = '{20, 21, 22};
    seen = 0;
    fork hold(queue[0]); join_none
    #1 queue = '{7};
    #2;
    if (seen !== 20) $fatal(1, "an element of a reassigned queue read %0d", seen);
    if (queue[0] !== 7)
      $fatal(1, "a write through an outdated reference reached the new queue: %0d", queue[0]);

    assoc[5] = 30;
    seen = 0;
    fork hold(assoc[5]); join_none
    #1 assoc.delete(5);
    #2;
    if (seen !== 30) $fatal(1, "a deleted associative entry read %0d", seen);
    if (assoc.exists(5))
      $fatal(1, "a write through a deleted entry made the entry again");

    texts = '{"a string long enough to own storage of its own, element zero"};
    seen_text = "";
    fork hold_text(texts[0]); join_none
    #1 texts.delete();
    #2;
    if (seen_text != "a string long enough to own storage of its own, element zero")
      $fatal(1, "a deleted string element read '%s'", seen_text);

    records = '{'{tag: 4, text: "a string long enough to own storage, in a structure"}};
    seen_record = '{tag: 0, text: ""};
    fork hold_record(records[0]); join_none
    #1 records.delete(0);
    #2;
    if (seen_record.tag !== 4 ||
        seen_record.text != "a string long enough to own storage, in a structure")
      $fatal(1, "a deleted structure element read tag %0d", seen_record.tag);

    queue = '{20, 21, 22};
    seen = 0;
    seen_second = 0;
    fork
      hold(queue[1]);
      hold_longer(queue[1]);
    join_none
    #1 void'(queue.pop_front());
    queue.delete(0);
    #4;
    if (seen !== 21) $fatal(1, "the first of two holders read %0d", seen);
    if (seen_second !== 99)
      $fatal(1, "the second of two holders read %0d after the first wrote", seen_second);

    queue = '{20, 21, 22};
    seen = 0;
    fork begin
      held = process::self();
      hold_forever(queue[1]);
    end join_none
    #1 held.kill();
    queue.delete();
    #1;
    if (seen !== 0) $fatal(1, "a killed holder ran on and read %0d", seen);

    queue = '{20, 21, 22};
    seen = 0;
    fork begin : holding
      hold_forever(queue[1]);
    end join_none
    #1 disable holding;
    queue.delete();
    #1;
    if (seen !== 0) $fatal(1, "a disabled holder ran on and read %0d", seen);

    $display("All checks passed");
  end
endmodule
