// A property is reached through a handle with the usual dot notation (LRM
// 8.4), and the handle is an operand of that access like any other: whatever
// side effects evaluating it has occur as the standard's rules give them (LRM
// 11.3.5), and an assignment operator evaluates its left-hand side once (LRM
// 11.4.1). So a function called to produce the handle runs once each time the
// access is evaluated -- for a read, a write, a write of an element or of a bit
// of the property, an assignment operator and a nonblocking assignment.
module Top;
  class Holder;
    int         plain;
    int         many [0:3];
    logic [7:0] bits;
  endclass

  Holder held;
  int    calls;

  function automatic Holder counted();
    calls = calls + 1;
    return held;
  endfunction

  int got;

  int on_read = -1;
  int on_write = -1;
  int on_element_write = -1;
  int on_bit_write = -1;
  int on_assignment_operator = -1;
  int on_nonblocking = -1;

  initial begin
    held = new;
    held.bits = 8'h00;

    calls = 0;
    counted().plain = 4;
    on_write = calls;

    calls = 0;
    got = counted().plain;
    on_read = calls;

    calls = 0;
    counted().many[2] = 9;
    on_element_write = calls;

    calls = 0;
    counted().bits[3] = 1'b1;
    on_bit_write = calls;

    calls = 0;
    counted().plain += 1;
    on_assignment_operator = calls;

    calls = 0;
    counted().plain <= 11;
    on_nonblocking = calls;
  end

  final begin
    if (on_write !== 1)
      $fatal(1, "a write ran the handle's call %0d times, expected 1",
             on_write);
    if (on_read !== 1)
      $fatal(1, "a read ran the handle's call %0d times, expected 1", on_read);
    if (on_element_write !== 1)
      $fatal(1,
             "a write of an element ran the handle's call %0d times, expected 1",
             on_element_write);
    if (on_bit_write !== 1)
      $fatal(1, "a write of a bit ran the handle's call %0d times, expected 1",
             on_bit_write);
    if (on_assignment_operator !== 1)
      $fatal(1,
             "an assignment operator ran the handle's call %0d times, expected 1",
             on_assignment_operator);
    if (on_nonblocking !== 1)
      $fatal(1,
             "a nonblocking assignment ran the handle's call %0d times, expected 1",
             on_nonblocking);
    // The accesses themselves took effect.
    if (got !== 4) $fatal(1, "the read answered %0d, expected 4", got);
    if (held.plain !== 11)
      $fatal(1, "the property ended as %0d, expected 11", held.plain);
    if (held.many[2] !== 9)
      $fatal(1, "the element ended as %0d, expected 9", held.many[2]);
    if (held.bits !== 8'h08)
      $fatal(1, "the bits ended as %h, expected 08", held.bits);
    $display("All checks passed");
  end
endmodule
