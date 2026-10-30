// A mailbox of a stated type (LRM 15.4.9) carries messages first in, first
// out (15.4.3). new() with no bound makes it unbounded and with a bound makes
// put() wait while it is full (15.4.1, 15.4.3); try_put() answers 0 where put()
// would wait (15.4.4). get() takes the oldest message or waits for one, and its
// waiters are served in the order they arrived (15.4.5); try_get() answers 0
// where get() would wait (15.4.6). peek() copies the oldest without taking it,
// and every peek waiting is let go by one message (15.4.7, 15.4.8). num() counts
// what is held (15.4.2). The message type may be any, a class or a type a
// design element declares included, and a class can extend a mailbox (15.2).
class Packet;
  int id;
  function new(int id);
    this.id = id;
  endfunction
endclass

class Counted extends mailbox #(int);
  int sent;
  function new();
    super.new(0);
  endfunction
  task send(int value);
    put(value);
    sent++;
  endtask
endclass

module Top;
  typedef struct {
    int a;
    string tag;
  } pair_t;

  mailbox #(int) bounded;
  mailbox #(Packet) packets;
  mailbox #(pair_t) pairs;
  mailbox #(string) words;
  Counted counted;

  int taken[$];
  time first_full_put = 0;
  time last_put = 0;
  int try_put_full = -1;
  int try_get_empty = -1;
  int num_when_full = -1;
  int peeked_a = -1;
  int peeked_b = -1;
  time peeks_done = 0;
  int num_after_peeks = -1;
  int packet_id = -1;
  pair_t pair = '{a: -1, tag: "unset"};
  string first_word = "unset";
  string second_word = "unset";
  int counted_value = -1;

  initial begin
    bounded = new(2);
    packets = new;
    pairs = new;
    words = new;
    counted = new;
    try_get_empty = bounded.try_get(counted_value);
    fork
      begin
        bounded.put(10);
        bounded.put(20);
        num_when_full = bounded.num();
        try_put_full = bounded.try_put(99);
        bounded.put(30);
        first_full_put = $time;
        bounded.put(40);
        last_put = $time;
      end
      begin
        int v;
        #1 bounded.get(v);
        taken.push_back(v);
        #1 bounded.get(v);
        taken.push_back(v);
        bounded.get(v);
        taken.push_back(v);
        bounded.get(v);
        taken.push_back(v);
      end
      begin
        Packet p;
        #2 packets.peek(p);
        peeked_a = p.id;
      end
      begin
        Packet p;
        #2 packets.peek(p);
        peeked_b = p.id;
        peeks_done = $time;
      end
      begin
        Packet p;
        #3 p = new(7);
        packets.put(p);
        #1 num_after_peeks = packets.num();
        packets.get(p);
        packet_id = p.id;
      end
      begin
        pairs.put('{a: 5, tag: "five"});
        pairs.get(pair);
      end
      begin
        words.get(first_word);
        words.get(second_word);
      end
      begin
        #1 words.put("one");
        words.put("two");
      end
      begin
        counted.send(42);
        counted.get(counted_value);
      end
    join
  end

  final begin
    if (try_get_empty !== 0)
      $fatal(1, "try_get on an empty mailbox answered %0d, expected 0", try_get_empty);
    if (num_when_full !== 2)
      $fatal(1, "num of a full mailbox was %0d, expected 2", num_when_full);
    if (try_put_full !== 0)
      $fatal(1, "try_put on a full mailbox answered %0d, expected 0", try_put_full);
    if (first_full_put !== 1 || last_put !== 2)
      $fatal(1, "puts into a full mailbox ended at %0t and %0t, expected 1 and 2",
             first_full_put, last_put);
    if (taken.size() !== 4 || taken[0] !== 10 || taken[1] !== 20 ||
        taken[2] !== 30 || taken[3] !== 40)
      $fatal(1, "messages came out of their order of arrival");
    if (peeked_a !== 7 || peeked_b !== 7 || peeks_done !== 3)
      $fatal(1, "two peeks saw %0d and %0d by %0t, expected 7 and 7 by 3",
             peeked_a, peeked_b, peeks_done);
    if (num_after_peeks !== 1)
      $fatal(1, "peeks left %0d messages, expected 1", num_after_peeks);
    if (packet_id !== 7)
      $fatal(1, "a class handle message came out as %0d, expected 7", packet_id);
    if (pair.a !== 5 || pair.tag != "five")
      $fatal(1, "a structure message came out as %0d %s", pair.a, pair.tag);
    if (first_word != "one" || second_word != "two")
      $fatal(1, "string messages came out as %s %s", first_word, second_word);
    if (counted_value !== 42 || counted.sent !== 1)
      $fatal(1, "a class extending a mailbox carried %0d (%0d sent)", counted_value,
             counted.sent);
    $display("All checks passed");
  end
endmodule
