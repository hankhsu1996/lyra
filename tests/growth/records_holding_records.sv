// A chain of N structures, each holding two members of the one before, so a
// variable of the last holds two to the N ints while its type states N + 1
// structures (LRM 7.2). Every part is an object a reader reaches by name and
// reads, writes, forces and releases (LRM 37.17, 38.15, 38.34), and what lets
// it is code for each structure, never for each part. The C++ path writes each
// structure the design names a file of its own.
// @each-adds: bytes, mir.callables 5, mir.constants 7, mir.records 1,
// @each-adds: mir.declared records 1, mir.record methods 15, mir.types 9,
// @each-adds: lir.constants 7, lir.functions 28, lir.records 2,
// @each-adds: lir.type steps 8, lir.types 11
// @each-adds: files 1 along cpp
module Top #(parameter int N = 4);
  typedef struct {
    int leaf;
  } [[t{i}; typedef struct { t{i} a, b; } |]] top_t;

  top_t held;
endmodule
