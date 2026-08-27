// Every expression in an assignment pattern is evaluated in the context of an
// assignment to the element or member it supplies rather than on its own, so
// a based literal wider than its target narrows to it while an unbased
// literal sets every bit of it, and a member declared signed holds what its
// own type makes of the bits even though the structure enclosing it is
// unsigned (LRM 10.9.1, 10.9.2, 5.7.1, 7.2.1).
module Top;
  typedef struct packed {
    logic signed [7:0] amount;
    logic [7:0] flags;
  } record_t;

  logic [1:0][3:0] from_based_literal = 8'hEE;
  logic [1:0][3:0] from_unbased_literal = 8'hEE;
  record_t record;

  int widened_amount;
  int widened_flags;
  int whole_record;

  initial begin
    from_based_literal = '{default: 'b1};
    from_unbased_literal = '{default: '1};

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
