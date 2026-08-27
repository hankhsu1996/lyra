// Each integer type fixes a width -- byte 8 bits, shortint 16, int and
// integer 32, longint and time 64 -- and a value assigned to one is truncated
// to that width. byte, shortint, int, integer and longint are signed while
// bit, logic and time are unsigned, and the signed and unsigned keywords
// override that default. Signedness decides whether a pattern with its
// topmost bit set counts as a negative number and whether it is sign-extended
// rather than zero-extended into a wider type, so one pattern held in two
// types orders on opposite sides of zero (LRM 6.11, Table 6-8, 6.11.2,
// 6.11.3).
module Top;
  byte truncated_byte;
  shortint truncated_shortint;
  int truncated_int;
  integer truncated_integer;
  longint kept_longint;
  time kept_time;
  int widened_signed_byte;
  int widened_unsigned_byte;
  int widened_unsigned_vector;
  int widened_signed_shortint;
  int widened_unsigned_logic;
  bit signed_int_below_zero;
  bit signed_int_above_zero;
  bit integer_below_zero;
  bit unsigned_int_above_zero;
  bit longint_below_zero;
  bit longint_above_zero;
  bit time_above_zero;
  bit time_below_zero;
  bit time_signed_below_zero;
  bit min_below_max;
  bit max_above_zero;

  initial begin
    byte all_ones_byte;
    byte unsigned all_ones_byte_unsigned;
    bit [7:0] all_ones_vector;
    shortint all_ones_shortint;
    logic [15:0] all_ones_logic;
    int all_ones_int;
    integer all_ones_integer;
    int unsigned all_ones_int_unsigned;
    longint all_ones_longint;
    time all_ones_time;
    time signed all_ones_time_signed;
    int min_int;
    int max_int;

    signed_int_above_zero = 1'b1;
    longint_above_zero = 1'b1;
    time_below_zero = 1'b1;

    truncated_byte = 32'h000001FF;
    truncated_shortint = 32'h12345678;
    truncated_int = 64'h0000000212345678;
    truncated_integer = 64'hFFFFFFFF87654321;
    kept_longint = 64'sd9876543210;
    kept_time = 64'hFFFFFFFFFFFFFFFF;

    // The same set bits assigned into a wider type from a signed source and
    // from an unsigned one, so only the extension rule separates the two. The
    // first pair differ in the keyword alone, the second in the type.
    all_ones_byte = 8'hFF;
    widened_signed_byte = all_ones_byte;
    all_ones_byte_unsigned = 8'hFF;
    widened_unsigned_byte = all_ones_byte_unsigned;
    all_ones_vector = 8'hFF;
    widened_unsigned_vector = all_ones_vector;
    all_ones_shortint = 16'hFFFF;
    widened_signed_shortint = all_ones_shortint;
    all_ones_logic = 16'hFFFF;
    widened_unsigned_logic = all_ones_logic;

    // The same 32 and 64 set bits ordered against zero in each type that can
    // hold them.
    all_ones_int = 32'hFFFFFFFF;
    signed_int_below_zero = (all_ones_int < 0);
    signed_int_above_zero = (all_ones_int > 0);
    all_ones_int_unsigned = 32'hFFFFFFFF;
    unsigned_int_above_zero = (all_ones_int_unsigned > 0);
    all_ones_integer = 32'hFFFFFFFF;
    integer_below_zero = (all_ones_integer < 0);

    all_ones_longint = 64'hFFFFFFFFFFFFFFFF;
    longint_below_zero = (all_ones_longint < 0);
    longint_above_zero = (all_ones_longint > 0);
    all_ones_time = 64'hFFFFFFFFFFFFFFFF;
    time_above_zero = (all_ones_time > 0);
    time_below_zero = (all_ones_time < 0);
    all_ones_time_signed = 64'hFFFFFFFFFFFFFFFF;
    time_signed_below_zero = (all_ones_time_signed < 0);

    min_int = 32'h80000000;
    max_int = 32'h7FFFFFFF;
    min_below_max = (min_int < max_int);
    max_above_zero = (max_int > 0);
  end

  final begin
    if (truncated_byte !== 8'hFF)
      $fatal(1, "truncated_byte was %h, expected ff", truncated_byte);
    if (truncated_shortint !== 16'h5678)
      $fatal(1, "truncated_shortint was %h, expected 5678",
             truncated_shortint);
    if (truncated_int !== 32'h12345678)
      $fatal(1, "truncated_int was %h, expected 12345678", truncated_int);
    if (truncated_integer !== 32'h87654321)
      $fatal(1, "truncated_integer was %h, expected 87654321",
             truncated_integer);
    if (kept_longint !== 64'sd9876543210)
      $fatal(1, "kept_longint was %0d, expected 9876543210", kept_longint);
    if (kept_time !== 64'hFFFFFFFFFFFFFFFF)
      $fatal(1, "kept_time was %h, expected all ones", kept_time);
    if (widened_signed_byte !== -1)
      $fatal(1, "widened_signed_byte was %0d, expected -1",
             widened_signed_byte);
    if (widened_unsigned_byte !== 255)
      $fatal(1, "widened_unsigned_byte was %0d, expected 255",
             widened_unsigned_byte);
    if (widened_unsigned_vector !== 255)
      $fatal(1, "widened_unsigned_vector was %0d, expected 255",
             widened_unsigned_vector);
    if (widened_signed_shortint !== -1)
      $fatal(1, "widened_signed_shortint was %0d, expected -1",
             widened_signed_shortint);
    if (widened_unsigned_logic !== 65535)
      $fatal(1, "widened_unsigned_logic was %0d, expected 65535",
             widened_unsigned_logic);
    if (signed_int_below_zero !== 1'b1)
      $fatal(1, "signed_int_below_zero was %b, expected 1",
             signed_int_below_zero);
    if (signed_int_above_zero !== 1'b0)
      $fatal(1, "signed_int_above_zero was %b, expected 0",
             signed_int_above_zero);
    if (integer_below_zero !== 1'b1)
      $fatal(1, "integer_below_zero was %b, expected 1", integer_below_zero);
    if (longint_below_zero !== 1'b1)
      $fatal(1, "longint_below_zero was %b, expected 1", longint_below_zero);
    if (longint_above_zero !== 1'b0)
      $fatal(1, "longint_above_zero was %b, expected 0", longint_above_zero);
    if (time_above_zero !== 1'b1)
      $fatal(1, "time_above_zero was %b, expected 1", time_above_zero);
    if (time_below_zero !== 1'b0)
      $fatal(1, "time_below_zero was %b, expected 0", time_below_zero);
    if (time_signed_below_zero !== 1'b1)
      $fatal(1, "time_signed_below_zero was %b, expected 1",
             time_signed_below_zero);
    if (min_below_max !== 1'b1)
      $fatal(1, "min_below_max was %b, expected 1", min_below_max);
    if (max_above_zero !== 1'b1)
      $fatal(1, "max_above_zero was %b, expected 1", max_above_zero);
    if (unsigned_int_above_zero !== 1'b1)
      $fatal(1, "unsigned_int_above_zero was %b, expected 1",
             unsigned_int_above_zero);
    $display("All checks passed");
  end
endmodule
