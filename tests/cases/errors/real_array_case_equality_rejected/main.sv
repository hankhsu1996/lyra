module Top;
  // The real-operand rejection propagates element-wise: an unpacked array with
  // a real leaf is rejected the same as a scalar real (LRM Table 11-1).
  real a[], b[];
  bit r;
  initial r = (a === b);
endmodule
