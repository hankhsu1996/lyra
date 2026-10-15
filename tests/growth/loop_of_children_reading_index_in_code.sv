// A generate loop whose blocks each hold an instance of one module handed its
// index, which only what the child's process selects requires a constant of
// (LRM 23.10, 27.4). What the child declares is one class whatever the index,
// so it is one unit, and each index is a body of it: lowered once, compiled
// once, and made by the design root, which states how an object of each body
// is made. Each body is a compiled unit of its own, holding its own code and
// stating the class the unit published, and reads the unit's signature once.
// N is how many blocks the loop counts out.
// @each-adds: units 1, files, bytes, bodies lowered 1,
// @each-adds: mir.classes, mir.constructors, mir.callables, mir.fields,
// @each-adds: mir.class constants, mir.integral constants, mir.types,
// @each-adds: mir.consumed signatures 1,
// @each-adds: lir.classes, lir.constants, lir.functions,
// @each-adds: lir.integral constants, lir.members, lir.types
module Child #(parameter int P = 0) (input logic [63:0] d, output logic [63:0] q);
  always_comb q = 64'(d[P:0]);
endmodule

module Top #(parameter int N = 4);
  logic [63:0] a;
  logic [63:0] y [N];
  for (genvar i = 0; i < N; i += 1) begin : g
    Child #(.P(i)) child (.d(a), .q(y[i]));
  end
endmodule
