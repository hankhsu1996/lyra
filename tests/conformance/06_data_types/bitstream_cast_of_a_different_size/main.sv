// @error: does not hold as many bits as its type
//
// A bit-stream cast whose operand holds a different number of bits than the
// casting type is an error, and where the operand's width is the running
// program's, one byte of a queue cast to an int here, the error is raised when
// the cast runs (LRM 6.24.3).
module Top;
  typedef int Word;

  Word word;

  initial begin
    byte bytes[$];
    bytes = {8'h01};
    word = Word'(bytes);
  end
endmodule
