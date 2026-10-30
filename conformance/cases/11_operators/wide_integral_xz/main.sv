// If any bit of an operand of an arithmetic operator is x or z the entire
// result is x, and a relational operator with such an operand yields 1'bx.
// A shift by an unknown amount is unknown, while shifting a known operand
// only fills with zeros. The rule reads the whole operand, so one unknown
// bit anywhere in a wide value makes the whole result unknown
// (LRM 11.4.3, 11.4.4, 11.4.10).
module Top;
  logic [127:0] sum;
  logic [127:0] difference;
  logic [127:0] product;
  logic [127:0] negation;
  logic [127:0] shifted_unknown;
  logic [127:0] shift_by_unknown;
  logic relational;
  logic [127:0] one_unknown_bit_in_high_word;
  logic [127:0] shifted_known;

  initial begin
    sum = 128'd0;
    difference = 128'd0;
    product = 128'd0;
    negation = 128'd0;
    shift_by_unknown = 128'd0;
    relational = 1'b0;
    one_unknown_bit_in_high_word = 128'd0;

    begin
      logic [127:0] known;
      logic [127:0] unknown;
      logic [127:0] mostly_known;

      known = 128'd100;
      unknown = 128'bxxxx;

      sum = known + unknown;
      difference = known - unknown;
      product = known * unknown;
      negation = -unknown;
      shifted_unknown = unknown << 4;
      shift_by_unknown = known >> unknown;
      relational = (known < unknown);

      mostly_known = 128'd0;
      mostly_known[100] = 1'bx;
      one_unknown_bit_in_high_word = known + mostly_known;

      shifted_known = 128'hFF << 60;
    end
  end

  final begin
    if (sum !== {128{1'bx}})
      $fatal(1, "adding an unknown gave %h, expected all x", sum);
    if (difference !== {128{1'bx}})
      $fatal(1, "subtracting an unknown gave %h, expected all x", difference);
    if (product !== {128{1'bx}})
      $fatal(1, "multiplying by an unknown gave %h, expected all x", product);
    if (negation !== {128{1'bx}})
      $fatal(1, "negating an unknown gave %h, expected all x", negation);
    if (shift_by_unknown !== {128{1'bx}})
      $fatal(1, "shifting by an unknown gave %h, expected all x",
             shift_by_unknown);
    if (one_unknown_bit_in_high_word !== {128{1'bx}})
      $fatal(1, "one unknown bit gave %h, expected all x",
             one_unknown_bit_in_high_word);
    if (relational !== 1'bx)
      $fatal(1, "comparing against an unknown was %b, expected x", relational);
    if (shifted_unknown !== {{124{1'bx}}, 4'b0000})
      $fatal(1, "shifting an unknown left gave %h, expected x bits over zeros",
             shifted_unknown);
    if (shifted_known !== 128'h000000000000000FF000000000000000)
      $fatal(1, "ff << 60 gave %h", shifted_known);
    $display("All checks passed");
  end
endmodule
