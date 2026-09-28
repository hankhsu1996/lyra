// An event control may watch a member of a class object reached through a
// handle (LRM 9.4.2): a change to that member is an event, and the object the
// member belongs to is whichever one the handle names. The standard's own
// example writes `@(p.status)` beside `@p`; a write to the member ends the
// first wait and a write of the handle ends the second.
class Packet;
  int status = 0;
endclass

module Top;
  Packet p;
  time member_woke = 0;
  time handle_woke = 0;

  initial begin
    p = new;
    fork
      begin
        @(p.status);
        member_woke = $time;
      end
      begin
        @p;
        handle_woke = $time;
      end
    join_none
    #5 p.status = 1;
    #5 p = new;
  end

  final begin
    if (member_woke !== 5) $fatal(1, "the member wait ended at %0d, expected 5", member_woke);
    if (handle_woke !== 10) $fatal(1, "the handle wait ended at %0d, expected 10", handle_woke);
    $display("All checks passed");
  end
endmodule
