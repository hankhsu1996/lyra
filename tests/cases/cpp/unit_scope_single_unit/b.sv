// Under single-unit compilation this module shares the other file's `$unit`
// scope, so it reads `shared` declared there by a bare name.
module Top;
  initial $display("%0d", shared);
endmodule
