// A generate loop whose blocks each hold an instance of one module handed a
// width one more than the block before handed (LRM 23.10, 27.4). The width
// sizes the child's port, so each child is a module of its own: a unit, with
// the one class its instances are, lowered once, read once by the module
// holding it, and named once by the design root, which states how an object of
// each unit is made. N is how many blocks the loop counts out.
// @each-adds: units 1, files, bytes, bodies lowered 1,
// @each-adds: mir.classes 1, mir.constructors 1, mir.callables, mir.fields,
// @each-adds: mir.types, mir.consumed signatures 2, mir.declared classes 2,
// @each-adds: lir.classes 1, lir.functions, lir.members, lir.types,
// @each-adds: lir.declared classes 2
module Child #(parameter int W = 1) (input logic [W-1:0] d, output logic q);
  assign q = ^d;
endmodule

module Top #(parameter int N = 4);
  logic [N-1:0] a;
  logic [N-1:0] y;
  for (genvar i = 0; i < N; i += 1) begin : g
    Child #(.W(i + 1)) child (.d(a[i:0]), .q(y[i]));
  end
endmodule
