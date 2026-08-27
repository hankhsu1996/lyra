// If any member of a packed structure is four-state, the structure as a whole
// is a four-state vector. A two-state member then occupies four-state bits,
// so reading it converts any unknown bit to zero and writing it stores known
// bits into the shared vector. A member declared signed is read as a signed
// value, and a member of a nested packed structure is reached the same way
// (LRM 6.11.2, 7.2.1).
module Top;
  typedef struct packed {
    bit [7:0] lo;
    bit signed [3:0] sgn;
    logic [7:0] hi;
  } mixed_t;

  typedef struct packed {
    bit [3:0] inner_lo;
    logic [3:0] inner_hi;
  } inner_t;

  typedef struct packed {
    inner_t inner;
    logic [7:0] top;
  } outer_t;

  logic [7:0] default_two_state;
  logic [7:0] default_four_state;
  logic [7:0] read_lo;
  logic [7:0] read_hi;
  logic signed [31:0] read_sgn;
  logic [19:0] whole;
  logic [3:0] nested_two_state;

  initial begin
    mixed_t s;
    outer_t o;

    // A four-state structure starts at x, so the two members show what the
    // read converts and what it does not.
    default_two_state = s.lo;
    default_four_state = s.hi;

    s.lo = 8'd200;
    s.sgn = -4'sd3;
    s.hi = 8'hFF;
    read_lo = s.lo;
    read_hi = s.hi;
    read_sgn = s.sgn;
    whole = s;

    o.inner.inner_lo = 4'd9;
    nested_two_state = o.inner.inner_lo;
  end

  final begin
    if (default_two_state !== 8'h00)
      $fatal(1, "default_two_state was %b, expected 00000000",
             default_two_state);
    if (default_four_state !== 8'bxxxxxxxx)
      $fatal(1, "default_four_state was %b, expected xxxxxxxx",
             default_four_state);
    if (read_lo !== 8'd200)
      $fatal(1, "read_lo was %0d, expected 200", read_lo);
    if (read_hi !== 8'hFF)
      $fatal(1, "read_hi was %h, expected ff", read_hi);
    if (read_sgn !== -3)
      $fatal(1, "read_sgn was %0d, expected -3", read_sgn);
    if (whole !== 20'hC8DFF)
      $fatal(1, "whole was %h, expected c8dff", whole);
    if (nested_two_state !== 4'd9)
      $fatal(1, "nested_two_state was %0d, expected 9", nested_two_state);
    $display("All checks passed");
  end
endmodule
