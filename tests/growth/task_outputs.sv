// A task of N outputs, each written by the task and copied out to its own
// variable when the task returns.
// @each-adds: bytes,
// @each-adds: mir.fields 1, lir.members 1,
// @each-adds: mir.integral constants 1, lir.integral constants 1,
// @each-adds: mir.class constants 1, lir.constants 1
module Top #(parameter int N = 4);
  task automatic t([[output int x{i}|, ]]);
    [[x{i} = {i};| ]]
  endtask

  int [[v{i}|, ]];

  initial t([[v{i}|, ]]);
endmodule
