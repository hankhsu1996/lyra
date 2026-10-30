// A parameter is a named data object that never changes: its value is fixed
// during elaboration, before simulation starts. A parameter declared with an
// explicit data type is a constant of that type, and it may be used wherever
// a constant of that type may -- the declaration initializer of a variable, a
// procedural expression, the bound of a loop -- and a later parameter's value
// may be an expression over an earlier one (LRM 6.20, 6.20.2).
module Top;
  parameter int COUNT = 5;
  parameter logic [7:0] MASK = 8'hA9;
  parameter real SCALE = 1.5;
  parameter string LABEL = "hello";
  parameter int LIMIT = COUNT - 1;

  int scaled_decl = COUNT * 3;
  logic [7:0] low_nibble = MASK & 8'h0F;
  real doubled_decl = SCALE * 2.0;
  string label_decl = LABEL;

  int in_procedure;
  int in_branch;
  int loop_total;
  real in_real_expr;
  string joined;

  initial begin
    in_procedure = COUNT + 1;
    if (COUNT > 0) in_branch = COUNT * 2;
    else in_branch = -1;
    loop_total = 0;
    for (int i = 0; i < LIMIT; i = i + 1) loop_total = loop_total + i;
    in_real_expr = SCALE + 0.5;
    joined = {LABEL, " world"};
  end

  final begin
    if (scaled_decl !== 15)
      $fatal(1, "scaled_decl was %0d, expected 15", scaled_decl);
    if (low_nibble !== 8'h09)
      $fatal(1, "low_nibble was %h, expected 09", low_nibble);
    if (doubled_decl != 3.0)
      $fatal(1, "doubled_decl was %g, expected 3.0", doubled_decl);
    if (label_decl != "hello")
      $fatal(1, "label_decl was '%s', expected 'hello'", label_decl);
    if (in_procedure !== 6)
      $fatal(1, "in_procedure was %0d, expected 6", in_procedure);
    if (in_branch !== 10)
      $fatal(1, "in_branch was %0d, expected 10", in_branch);
    if (loop_total !== 6)
      $fatal(1, "loop_total was %0d, expected 6", loop_total);
    if (in_real_expr != 2.0)
      $fatal(1, "in_real_expr was %g, expected 2.0", in_real_expr);
    if (joined != "hello world")
      $fatal(1, "joined was '%s', expected 'hello world'", joined);
    $display("All checks passed");
  end
endmodule
