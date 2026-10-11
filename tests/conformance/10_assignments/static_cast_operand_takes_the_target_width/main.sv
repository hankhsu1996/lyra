// A static cast is an assignment-like context, as if to a variable of the
// target type, so its operand is evaluated at the target's width: an unsized
// single-bit literal fills it, and an operation is carried out at that width
// and keeps the bits its own operands are too narrow for. That holds for an
// enumerated target as for a vector, though an integral expression could not
// be assigned to an enumerated variable without the cast (LRM 10.8, 5.7.1,
// 11.6.1, 6.19.4).
module Top;
  typedef logic [2:0] vec_t;
  typedef enum logic [2:0] {NONE = 3'd0, ONE = 3'd1, ALL = 3'd7} state_t;
  typedef enum {FIRST, SECOND} count_t;

  logic [1:0] three = 2'd3;
  logic [1:0] one = 2'd1;
  logic bit_one = 1'b1;

  vec_t vec_fill = 3'd2;
  vec_t vec_sum = 3'd2;
  state_t enum_fill = ONE;
  state_t enum_sum = ONE;
  state_t enum_shift = ONE;
  state_t enum_from_enum = NONE;
  count_t wide_fill = SECOND;
  // The masks a design builds from a cast: every bit of the member the cast
  // names, beside every bit of a plain vector.
  struct packed {
    state_t state;
    logic [3:0] tag;
  } mask = '0;

  initial begin
    vec_fill = vec_t'('1);
    vec_sum = vec_t'(three + one);
    enum_fill = state_t'('1);
    enum_sum = state_t'(three + one);
    enum_shift = state_t'(bit_one << 2);
    enum_from_enum = state_t'(SECOND);
    wide_fill = count_t'('1);
    mask = '{state: state_t'('1), tag: '1};
  end

  final begin
    if (vec_fill !== 3'b111) $fatal(1, "vec_fill was %b, expected 111", vec_fill);
    if (vec_sum !== 3'b100) $fatal(1, "vec_sum was %b, expected 100", vec_sum);
    if (enum_fill !== ALL) $fatal(1, "enum_fill was %b, expected 111", enum_fill);
    if (enum_sum !== 3'b100)
      $fatal(1, "enum_sum was %b, expected 100", enum_sum);
    if (enum_shift !== 3'b100)
      $fatal(1, "enum_shift was %b, expected 100", enum_shift);
    if (enum_from_enum !== ONE)
      $fatal(1, "enum_from_enum was %b, expected 001", enum_from_enum);
    if (wide_fill !== 32'hffff_ffff)
      $fatal(1, "wide_fill was %h, expected ffffffff", wide_fill);
    if (mask !== 7'b111_1111) $fatal(1, "mask was %b, expected 1111111", mask);
    $display("All checks passed");
  end
endmodule
