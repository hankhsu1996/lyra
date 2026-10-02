// A member of a tagged union is read or assigned with the usual dot notation,
// and the access is checked against the current tag (LRM 11.9). The check is
// part of evaluating the access and not a second evaluation of it: the side
// effects of evaluating an operand occur as the standard's rules give them
// (LRM 11.3.5), and an index on the left-hand side of an assignment operator
// is evaluated once (LRM 11.4.1). So a function called in the index that
// reaches the union runs once each time the access is evaluated -- for a read,
// a write, a write of part of the member, an assignment operator, an increment
// and a nonblocking assignment, and for an unpacked union as for a packed one.
module Top;
  typedef union tagged packed {
    logic [3:0] a;
    logic [3:0] b;
  } packed_choice_t;

  typedef union tagged {
    int a;
    int b;
  } unpacked_choice_t;

  packed_choice_t   packed_choices [0:3];
  unpacked_choice_t unpacked_choices [0:3];

  int calls;

  function automatic int counted();
    calls = calls + 1;
    return 1;
  endfunction

  logic [3:0] got;
  int         got_int;

  int on_read;
  int on_write;
  int on_bit_write;
  int on_assignment_operator;
  int on_increment;
  int on_nonblocking;
  int on_unpacked_read;
  int on_unpacked_write;

  initial begin
    packed_choices[1] = tagged b 4'h0;
    unpacked_choices[1] = tagged b 0;

    calls = 0;
    got = packed_choices[counted()].b;
    on_read = calls;

    calls = 0;
    packed_choices[counted()].b = 4'h5;
    on_write = calls;

    calls = 0;
    packed_choices[counted()].b[1] = 1'b1;
    on_bit_write = calls;

    calls = 0;
    packed_choices[counted()].b += 4'h1;
    on_assignment_operator = calls;

    calls = 0;
    packed_choices[counted()].b++;
    on_increment = calls;

    calls = 0;
    packed_choices[counted()].b <= 4'h2;
    on_nonblocking = calls;

    calls = 0;
    got_int = unpacked_choices[counted()].b;
    on_unpacked_read = calls;

    calls = 0;
    unpacked_choices[counted()].b = 3;
    on_unpacked_write = calls;
  end

  final begin
    if (on_read !== 1)
      $fatal(1, "a read ran the index %0d times, expected 1", on_read);
    if (on_write !== 1)
      $fatal(1, "a write ran the index %0d times, expected 1", on_write);
    if (on_bit_write !== 1)
      $fatal(1, "a write of one bit ran the index %0d times, expected 1",
             on_bit_write);
    if (on_assignment_operator !== 1)
      $fatal(1, "an assignment operator ran the index %0d times, expected 1",
             on_assignment_operator);
    if (on_increment !== 1)
      $fatal(1, "an increment ran the index %0d times, expected 1",
             on_increment);
    if (on_nonblocking !== 1)
      $fatal(1, "a nonblocking assignment ran the index %0d times, expected 1",
             on_nonblocking);
    if (on_unpacked_read !== 1)
      $fatal(1, "a read of an unpacked union ran the index %0d times, expected 1",
             on_unpacked_read);
    if (on_unpacked_write !== 1)
      $fatal(1,
             "a write of an unpacked union ran the index %0d times, expected 1",
             on_unpacked_write);
    // The accesses themselves took effect: 5, then bit 1 set, plus 1, plus 1,
    // then the nonblocking 2 landed.
    if (packed_choices[1].b !== 4'h2)
      $fatal(1, "the packed member ended as %h, expected 2",
             packed_choices[1].b);
    if (got !== 4'h0)
      $fatal(1, "the read answered %h, expected 0", got);
    if (unpacked_choices[1].b !== 3)
      $fatal(1, "the unpacked member ended as %0d, expected 3",
             unpacked_choices[1].b);
    $display("All checks passed");
  end
endmodule
