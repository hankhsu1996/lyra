// An event control may watch a member of a class object reached through a
// handle (LRM 9.4.2): a change to that member is an event, and the object the
// member belongs to is whichever one the handle names. The standard's own
// example writes `@(p.status)` beside `@p`; a write to the member ends the
// first wait and a write of the handle ends the second. After the handle is
// written, the member wait watches the new object, and the write itself is an
// event exactly when the new object's member already differs from what the
// old one held, and a write to the old object no longer reaches it.
//
// Every way of writing a member reaches the wait: an assignment, a compound
// assignment, an increment, a nonblocking assignment, a built-in method that
// changes a member, and a task writing through a `ref` bound to the member.
// A member named bare inside a method is a member of the object the method
// runs on, and a wait may go through a handle a member holds. A wait reading
// two members of one object is reached through both by one write, which is
// one event.
class Packet;
  int status = 0;
  int other = 0;
  int q[$];
  Packet next;

  task automatic AwaitStatus(output time woke);
    @(status);
    woke = $time;
  endtask
endclass

module Top;
  Packet p;
  Packet same;
  Packet differs;
  Packet chain;
  time member_woke = 0;
  time handle_woke = 0;
  time rebind_same_woke = 0;
  time rebind_differs_woke = 0;
  time bare_woke = 0;
  time compound_woke = 0;
  time increment_woke = 0;
  time nonblocking_woke = 0;
  time method_woke = 0;
  time ref_woke = 0;
  time level_woke = 0;
  time chained_woke = 0;
  time edge_woke = 0;
  time twice_woke = 0;
  Packet twice;
  Packet a;
  Packet a_first;
  Packet b;
  Packet c;
  Packet d;
  Packet e;
  Packet f;
  Packet g;
  Packet h;
  Packet k;

  task automatic Poke(ref int target);
    target = 9;
  endtask

  initial begin
    p = new;
    same = new;
    differs = new;
    differs.status = 4;
    a = new;
    a_first = a;
    b = new;
    c = new;
    d = new;
    e = new;
    f = new;
    g = new;
    h = new;
    k = new;
    chain = new;
    chain.next = new;
    twice = new;
    fork
      begin
        @(p.status);
        member_woke = $time;
      end
      begin
        @p;
        handle_woke = $time;
      end
      begin
        @(a.status);
        rebind_same_woke = $time;
      end
      begin
        @(b.status);
        rebind_differs_woke = $time;
      end
      c.AwaitStatus(bare_woke);
      begin
        @(d.status);
        compound_woke = $time;
      end
      begin
        @(e.status);
        increment_woke = $time;
      end
      begin
        @(f.status);
        nonblocking_woke = $time;
      end
      begin
        @(g.q.size());
        method_woke = $time;
      end
      begin
        @(h.status);
        ref_woke = $time;
      end
      begin
        wait (k.status == 2);
        level_woke = $time;
      end
      begin
        @(chain.next.status);
        chained_woke = $time;
      end
      begin
        @(posedge p.status[0]);
        edge_woke = $time;
      end
      // Two members of one object: the one write reaches the wait through
      // both, and is one event.
      begin
        @(twice.status + twice.other);
        twice_woke = $time;
      end
    join_none
    #5 p.status = 1;
    #5 p = new;
    // A handle written to name an object whose member holds what the old one
    // held is no event; a later write to the old object is not watched any
    // more, and one to the new object is.
    #5 a = same;
    #5 begin
      a_first.status = 7;
      same.status = 0;
    end
    #5 same.status = 6;
    // A handle written to name an object whose member already differs ends the
    // wait at that write.
    #5 b = differs;
    #5 c.status = 1;
    #5 d.status += 2;
    #5 e.status++;
    #5 f.status <= 3;
    #5 g.q.push_back(1);
    #5 Poke(h.status);
    #5 k.status = 1;
    #5 k.status = 2;
    // The handle the member holds is written, then the member of the object it
    // now names.
    #5 chain.next = new;
    #5 chain.next.status = 5;
    #5 twice.other = 1;
  end

  final begin
    if (member_woke !== 5) $fatal(1, "the member wait ended at %0t, expected 5", member_woke);
    if (handle_woke !== 10) $fatal(1, "the handle wait ended at %0t, expected 10", handle_woke);
    if (edge_woke !== 5) $fatal(1, "the edge wait ended at %0t, expected 5", edge_woke);
    if (rebind_same_woke !== 25) $fatal(1, "the wait rebound to an equal member ended at %0t, expected 25", rebind_same_woke);
    if (rebind_differs_woke !== 30) $fatal(1, "the wait rebound to a different member ended at %0t, expected 30", rebind_differs_woke);
    if (bare_woke !== 35) $fatal(1, "the wait on a member named in a method ended at %0t, expected 35", bare_woke);
    if (compound_woke !== 40) $fatal(1, "the wait on a compound write ended at %0t, expected 40", compound_woke);
    if (increment_woke !== 45) $fatal(1, "the wait on an increment ended at %0t, expected 45", increment_woke);
    if (nonblocking_woke !== 50) $fatal(1, "the wait on a nonblocking write ended at %0t, expected 50", nonblocking_woke);
    if (method_woke !== 55) $fatal(1, "the wait on a queue's size ended at %0t, expected 55", method_woke);
    if (ref_woke !== 60) $fatal(1, "the wait on a write through a ref ended at %0t, expected 60", ref_woke);
    if (level_woke !== 70) $fatal(1, "the level wait ended at %0t, expected 70", level_woke);
    if (chained_woke !== 80) $fatal(1, "the wait through a member handle ended at %0t, expected 80", chained_woke);
    if (twice_woke !== 85) $fatal(1, "the wait on two members of one object ended at %0t, expected 85", twice_woke);
    $display("All checks passed");
  end
endmodule
