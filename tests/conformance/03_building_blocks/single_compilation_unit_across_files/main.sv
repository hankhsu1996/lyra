// @args: --single-unit
//
// A conforming tool offers a use model in which all the files on one
// compilation command line make a single compilation unit, and a declaration in
// that unit's scope is then visible by the normal rules across the whole set of
// files (LRM 3.12.1).
module Top;
  int seen;

  initial seen = shared;

  final begin
    if (seen !== 42) $fatal(1, "seen was %0d, expected 42", seen);
    $display("All checks passed");
  end
endmodule
