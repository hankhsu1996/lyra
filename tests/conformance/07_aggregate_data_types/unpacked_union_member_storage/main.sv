// An unpacked union is one piece of storage that is read and written through
// one of its named member types, and only one of those members is in use at a
// time. A variable of an unpacked union type declared without an initializer
// holds the default initial value for the type of the union's first member in
// declaration order, so which member is written first in the type is what
// decides whether a fresh variable reads as zero, as x, or as 0.0. A member
// name is a variable of the member's type: it is a target, an operand of a
// read-modify-write, and the name a bit-select or a further member select
// applies to. Assigning one union variable to another copies the storage, and
// the two are independent afterwards (LRM 7.3, 6.8, Table 6-7).
module Top;
  typedef struct {
    int low;
    int high;
  } pair_t;

  typedef union {
    logic [15:0] unknown_first;
    int number;
  } four_state_first_t;

  typedef union {
    int number;
    logic [15:0] bits;
  } two_state_first_t;

  typedef union {
    shortreal fraction;
    int number;
  } real_first_t;

  typedef union {
    pair_t pair;
    logic [15:0] bits;
  } aggregate_first_t;

  four_state_first_t unknown_default;
  two_state_first_t zero_default;
  real_first_t real_default;
  aggregate_first_t aggregate_default;

  two_state_first_t stored;
  two_state_first_t bit_user;
  aggregate_first_t nested;
  two_state_first_t original;
  two_state_first_t duplicate;

  logic [15:0] read_unknown_default = 16'h5A5A;
  int read_zero_default = 77;
  shortreal read_real_default = 1.5;
  int read_aggregate_default = 77;

  int read_stored;
  int read_after_increase;
  logic [15:0] read_bits;
  logic [15:0] read_bits_after_set;
  int read_nested_low;
  int read_nested_high;
  int read_duplicate;
  int read_duplicate_after_change;

  initial begin
    read_unknown_default = unknown_default.unknown_first;
    read_zero_default = zero_default.number;
    read_real_default = real_default.fraction;
    read_aggregate_default = aggregate_default.pair.low;

    stored.number = 42;
    read_stored = stored.number;
    stored.number += 5;
    read_after_increase = stored.number;

    bit_user.bits = 16'hBEEF;
    read_bits = bit_user.bits;
    bit_user.bits[4] = 1'b1;
    read_bits_after_set = bit_user.bits;

    nested.pair.low = 11;
    nested.pair.high = 22;
    read_nested_low = nested.pair.low;
    read_nested_high = nested.pair.high;

    original.number = 42;
    duplicate = original;
    read_duplicate = duplicate.number;
    original.number = 7;
    read_duplicate_after_change = duplicate.number;
  end

  final begin
    if (read_unknown_default !== 16'bx)
      $fatal(1, "read_unknown_default was %0h, expected all x",
             read_unknown_default);
    if (read_zero_default !== 0)
      $fatal(1, "read_zero_default was %0d, expected 0", read_zero_default);
    if (read_real_default != 0.0)
      $fatal(1, "read_real_default was %f, expected 0.0", read_real_default);
    if (read_aggregate_default !== 0)
      $fatal(1, "read_aggregate_default was %0d, expected 0",
             read_aggregate_default);

    if (read_stored !== 42)
      $fatal(1, "read_stored was %0d, expected 42", read_stored);
    if (read_after_increase !== 47)
      $fatal(1, "read_after_increase was %0d, expected 47",
             read_after_increase);

    if (read_bits !== 16'hBEEF)
      $fatal(1, "read_bits was %0h, expected beef", read_bits);
    if (read_bits_after_set !== 16'hBEFF)
      $fatal(1, "read_bits_after_set was %0h, expected beff",
             read_bits_after_set);

    if (read_nested_low !== 11)
      $fatal(1, "read_nested_low was %0d, expected 11", read_nested_low);
    if (read_nested_high !== 22)
      $fatal(1, "read_nested_high was %0d, expected 22", read_nested_high);

    if (read_duplicate !== 42)
      $fatal(1, "read_duplicate was %0d, expected 42", read_duplicate);
    if (read_duplicate_after_change !== 42)
      $fatal(1, "read_duplicate_after_change was %0d, expected 42",
             read_duplicate_after_change);
    $display("All checks passed");
  end
endmodule
