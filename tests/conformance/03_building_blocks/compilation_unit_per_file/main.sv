// A conforming tool offers a use model in which each source file is a separate
// compilation unit, and then the declarations in a file's compilation-unit
// scope are accessible only within that file (LRM 3.12.1). Two files declaring
// the same name at that level therefore declare two objects in two scopes, and
// a design element sees the one its own file declares.
int m = 7;

module Top;
  int seen;

  initial seen = m;

  final begin
    if (seen !== 7) $fatal(1, "seen was %0d, expected 7", seen);
    $display("All checks passed");
  end
endmodule
