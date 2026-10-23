// @error: does not divide into the elements
//
// A bit-stream cast to a dynamically sized type sizes it to hold exactly the
// operand's bits, so an operand whose bits do not divide into whole elements
// is an error, raised when the cast runs: twelve bits are not a whole number
// of bytes (LRM 6.24.3).
module Top;
  typedef byte Bytes[$];
  typedef bit [3:0] Nibble;

  Bytes bytes;

  initial begin
    Nibble nibbles[$];
    nibbles = {4'h1, 4'h2, 4'h3};
    bytes = Bytes'(nibbles);
  end
endmodule
