// Per-file compilation (the default): each file is its own compilation-unit
// input, so each file's `$unit` scope (LRM 3.12.1) is separate. This file's
// `m` is invisible to the other file's `$unit`, and the two same-named `$unit`
// variables are distinct cells in distinct anonymous units -- they must not
// collide. `Top` reads its own file's `m`.
int m = 7;

module Top;
  initial $display("%0d", m);
endmodule
