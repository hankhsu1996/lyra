module Top;
  // LRM Table 11-1: case equality (=== / !==) is not defined on real /
  // shortreal operands. slang accepts and evaluates it as `==`, but the backend
  // follows the LRM strictly and rejects it in lowering.
  real a, b;
  bit r;
  initial r = (a === b);
endmodule
