// A write to a member of a tagged union is checked against the union's tag
// (LRM 11.9), and the check belongs to the assignment, which is made where the
// write lands. A nonblocking assignment lands when its update is applied (LRM
// 10.4.2), and an output argument when the task returns and its value is copied
// back (LRM 13.5). So such a write lands without error where the tag names the
// member by then, though the union held another member when the statement was
// reached -- for an unpacked union as for a packed one.
module Top;
  typedef union tagged {
    int a;
    int b;
  } unpacked_choice_t;

  typedef union tagged packed {
    logic [7:0] a;
    logic [7:0] b;
  } packed_choice_t;

  unpacked_choice_t unpacked_late;
  packed_choice_t packed_late;
  unpacked_choice_t unpacked_out;
  packed_choice_t packed_out;

  task automatic set_later(output int to);
    #1 to = 41;
  endtask

  task automatic set_bits_later(output logic [7:0] to);
    #1 to = 8'h42;
  endtask

  initial begin
    unpacked_late = tagged b 1;
    packed_late = tagged b 8'h1;
    unpacked_late.a <= 7;
    packed_late.a <= 8'h7;
    unpacked_late = tagged a 0;
    packed_late = tagged a 8'h0;

    unpacked_out = tagged b 2;
    packed_out = tagged b 8'h2;
    fork
      set_later(unpacked_out.a);
      set_bits_later(packed_out.a);
    join_none
    #0;
    unpacked_out = tagged a 0;
    packed_out = tagged a 8'h0;
  end

  final begin
    if (unpacked_late !== unpacked_choice_t'(tagged a 7))
      $fatal(1, "the nonblocking write to the unpacked union did not land");
    if (packed_late !== packed_choice_t'(tagged a 8'h7))
      $fatal(1, "the nonblocking write to the packed union landed as %p",
             packed_late);
    if (unpacked_out !== unpacked_choice_t'(tagged a 41))
      $fatal(1, "the output copied back to the unpacked union did not land");
    if (packed_out !== packed_choice_t'(tagged a 8'h42))
      $fatal(1, "the output copied back to the packed union landed as %p",
             packed_out);
    $display("All checks passed");
  end
endmodule
