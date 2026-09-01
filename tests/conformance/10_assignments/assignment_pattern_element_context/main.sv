// Every expression in an assignment pattern is evaluated in the context of an
// assignment to the element or member it supplies rather than on its own, so
// a based literal wider than its target narrows to it while an unbased
// literal sets every bit of it, and a member declared signed holds what its
// own type makes of the bits even though the structure enclosing it is
// unsigned. This holds for the one expression a default key supplies to every
// unmatched element as much as for a keyed one, and whether the elements are
// packed into a vector or are storage of their own (LRM 10.9.1, 10.9.2, 5.7.1,
// 7.2.1).
module Top;
  typedef struct packed {
    logic signed [7:0] amount;
    logic [7:0] flags;
  } record_t;

  logic [1:0][3:0] from_based_literal = 8'hEE;
  logic [1:0][3:0] from_unbased_literal = 8'hEE;
  logic [3:0] unpacked_from_wide_literal [0:1];
  logic [3:0] unpacked_from_unbased_literal [0:1];
  record_t record;

  int widened_amount;
  int widened_flags;
  int whole_record;

  initial begin
    from_based_literal = '{default: 'b1};
    from_unbased_literal = '{default: '1};
    unpacked_from_wide_literal = '{default: 8'hFF};
    unpacked_from_unbased_literal = '{default: '1};

    record = '{amount: 8'hFF, flags: 8'hFF};
    widened_amount = record.amount;
    widened_flags = record.flags;
    whole_record = record;
  end

  final begin
    if (from_based_literal !== 8'h11)
      $fatal(1, "a based literal filled the elements with %0h, expected 11",
             from_based_literal);
    if (from_unbased_literal !== 8'hFF)
      $fatal(1, "an unbased literal filled the elements with %0h, expected ff",
             from_unbased_literal);

    if (unpacked_from_wide_literal[0] !== 4'hF ||
        unpacked_from_wide_literal[1] !== 4'hF)
      $fatal(1, "a wide literal gave elements %0h %0h, expected f f",
             unpacked_from_wide_literal[0], unpacked_from_wide_literal[1]);
    if (unpacked_from_unbased_literal[0] !== 4'hF ||
        unpacked_from_unbased_literal[1] !== 4'hF)
      $fatal(1, "an unbased literal gave elements %0h %0h, expected f f",
             unpacked_from_unbased_literal[0],
             unpacked_from_unbased_literal[1]);

    if (widened_amount !== -1)
      $fatal(1, "a signed member widened to %0d, expected -1", widened_amount);
    if (widened_flags !== 255)
      $fatal(1, "an unsigned member widened to %0d, expected 255",
             widened_flags);
    if (whole_record !== 65535)
      $fatal(1, "the structure widened to %0d, expected 65535", whole_record);
    $display("All checks passed");
  end
endmodule
