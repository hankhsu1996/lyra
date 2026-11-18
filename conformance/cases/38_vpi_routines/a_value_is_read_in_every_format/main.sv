// `vpi_get_value` reads a variable in each format of LRM Table 38-3 that
// applies to it: an integral value as binary, octal, decimal and hexadecimal
// text -- an x or z digit where its bits are all x or z, and an X or Z where
// some are, either where a digit's bits hold both -- as a scalar, an integer
// with x and z read as 0, a real, the characters its bytes hold, a time, the
// aval and bval words of a vector and the strength of each bit, which a
// variable holds as strong (LRM 38.15); a real as a real; and a string as its
// characters. Asked for its own format, each object answers the one LRM 38.15
// names for its kind, a `longint` an integer's or a vector's, and a member of
// a packed structure the one its own type has. A part of a variable -- a
// member of a packed structure, an element of a packed array, a bit -- reads
// as the value it is, an element of a packed array unsigned whatever the
// array is (LRM 7.4.1), and so does an enumeration's constant and a range's
// bound. A value wider than 64 bits reads as a real as the design converts it,
// below zero where it is signed and negative, and lands on one of the two
// reals nearest it where none is the number, as a C double does (LRM 6.12,
// ISO C 6.3.1.4). A format no value of the kind is read in answers nothing
// and is the routine's error (LRM 38.2).
module Top;
  import "DPI-C" context function int look();

  typedef struct packed {
    bit [3:0] hi;
    bit [3:0] lo;
  } nibbles_t;
  typedef enum bit [1:0] {kRed, kGreen, kBlue} colour_t;

  logic [7:0] lv = 8'b1010_0011;
  logic [3:0] lx = 4'b10xz;
  logic sl = 1'b1;
  logic [15:0] text = "AB";
  int negative = -5;
  longint wide = 64'h1_0000_0002;
  time t = 64'h3_0000_0004;
  real r = 2.5;
  string s = "hi";
  nibbles_t ps = 8'hA5;
  struct packed {
    int count;
    bit [7:0] flags;
  } counted = {32'sd7, 8'h01};
  logic signed [1:0][7:0] signed_rows = 16'hff00;
  logic signed [99:0] wide_negative = -100'sd3;
  logic [128:0] rounded = (129'd1 << 128) | (129'd1 << 75) | 129'd1;
  struct packed signed {
    bit [3:0] nibble;
  } [1:0] signed_pairs = 8'hf0;
  nibbles_t [1:0] pa = 16'h1234;
  colour_t e = kBlue;
  logic [69:0] long_vector = {6'b10_1010, 64'hFFFF_0000_1234_5678};

  int differences = -1;

  initial #1 differences = look();

  final begin
    if (differences !== 0)
      $fatal(1, "the values differ in %0d answers", differences);
    $display("All checks passed");
  end
endmodule
