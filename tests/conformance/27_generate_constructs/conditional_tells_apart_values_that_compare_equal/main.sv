// Elaboration gives every parameter of an instance its final value and then
// evaluates each generate construct of that instance (LRM 23.10.4.1), and a
// conditional generate construct selects its block from a constant expression
// evaluated then (LRM 27.5). So two instances of one module select by the
// value each was given, also where the two values compare equal and a
// constant expression can still tell them apart.
//
// One such pair is the two zeros of a real. A real is represented as IEEE Std
// 754 describes (LRM 6.12), which has a zero of each sign: the two compare
// equal and differ in one bit. $bitstoreal gives the real a representation
// stands for and $realtobits the representation of a real, and both may be
// used in a constant expression (LRM 20.5), so each instance can be given one
// of the zeros exactly and can tell which it holds. The value reaches the
// condition as a parameter of its own, as an element of an unpacked array and
// as a member of an unpacked structure, which are the positions a real is
// held in differently; a shortreal has a representation of its own.
//
// The other is one number written at two sizes. A parameter declared with no
// type and no range is a vector the size of the value it is finally given
// (LRM 6.20.2), so 4'd1 and 8'd1 make it two types holding the same number,
// and $bits reads which (LRM 20.6.2).
//
// In every pair the instance written second holds the value the first does
// not, since an instance built as the one before it is seen only where the
// later one is checked. Each instance says what it selected through a port, so
// nothing outside it names anything inside it.
typedef struct {
  int  tag;
  real value;
} tagged_real;

module ByReal #(parameter real P = 1.0) (output int selected);
  if ($realtobits(P) == 64'h8000_0000_0000_0000) begin : g
    initial selected = 2;
  end else begin : g
    initial selected = 1;
  end
endmodule

module ByShortreal #(parameter shortreal P = 1.0) (output int selected);
  if ($shortrealtobits(P) == 32'h8000_0000) begin : g
    initial selected = 2;
  end else begin : g
    initial selected = 1;
  end
endmodule

module ByElement #(parameter real P[2] = '{1.0, 1.0}) (output int selected);
  if ($realtobits(P[1]) == 64'h8000_0000_0000_0000) begin : g
    initial selected = 2;
  end else begin : g
    initial selected = 1;
  end
endmodule

module ByMember #(parameter tagged_real P = '{0, 1.0}) (output int selected);
  if ($realtobits(P.value) == 64'h8000_0000_0000_0000) begin : g
    initial selected = 2;
  end else begin : g
    initial selected = 1;
  end
endmodule

module BySize #(parameter P = 1'b1) (output int selected);
  if ($bits(P) == 8) begin : g
    initial selected = 2;
  end else begin : g
    initial selected = 1;
  end
endmodule

module Top;
  localparam real PositiveZero = $bitstoreal(64'h0000_0000_0000_0000);
  localparam real NegativeZero = $bitstoreal(64'h8000_0000_0000_0000);
  localparam shortreal ShortPositiveZero = $bitstoshortreal(32'h0000_0000);
  localparam shortreal ShortNegativeZero = $bitstoshortreal(32'h8000_0000);

  int real_positive, real_negative;
  int shortreal_positive, shortreal_negative;
  int element_positive, element_negative;
  int member_positive, member_negative;
  int size_four, size_eight;

  ByReal #(PositiveZero) by_real_positive (.selected(real_positive));
  ByReal #(NegativeZero) by_real_negative (.selected(real_negative));

  ByShortreal #(ShortPositiveZero) by_shortreal_positive (
      .selected(shortreal_positive)
  );
  ByShortreal #(ShortNegativeZero) by_shortreal_negative (
      .selected(shortreal_negative)
  );

  ByElement #('{1.0, PositiveZero}) by_element_positive (
      .selected(element_positive)
  );
  ByElement #('{1.0, NegativeZero}) by_element_negative (
      .selected(element_negative)
  );

  ByMember #('{7, PositiveZero}) by_member_positive (
      .selected(member_positive)
  );
  ByMember #('{7, NegativeZero}) by_member_negative (
      .selected(member_negative)
  );

  BySize #(4'd1) by_size_four (.selected(size_four));
  BySize #(8'd1) by_size_eight (.selected(size_eight));

  final begin
    if (real_positive !== 1)
      $fatal(1, "a real of positive zero selected %0d", real_positive);
    if (real_negative !== 2)
      $fatal(1, "a real of negative zero selected %0d", real_negative);
    if (shortreal_positive !== 1)
      $fatal(1, "a shortreal of positive zero selected %0d",
             shortreal_positive);
    if (shortreal_negative !== 2)
      $fatal(1, "a shortreal of negative zero selected %0d",
             shortreal_negative);
    if (element_positive !== 1)
      $fatal(1, "an element of positive zero selected %0d", element_positive);
    if (element_negative !== 2)
      $fatal(1, "an element of negative zero selected %0d", element_negative);
    if (member_positive !== 1)
      $fatal(1, "a member of positive zero selected %0d", member_positive);
    if (member_negative !== 2)
      $fatal(1, "a member of negative zero selected %0d", member_negative);
    if (size_four !== 1)
      $fatal(1, "a one of four bits selected %0d", size_four);
    if (size_eight !== 2)
      $fatal(1, "a one of eight bits selected %0d", size_eight);
    $display("All checks passed");
  end
endmodule
