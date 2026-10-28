// @argv: +REAL=8.25
//
// %e, %f and %g are among the format strings $value$plusargs converts a matched
// plusarg by, so the remainder of the match is read as a real number and stored
// in the variable given (LRM 21.6).
module Top;
  int matched_real;
  real rate;

  initial begin
    rate = 1.0;
    matched_real = $value$plusargs("REAL=%f", rate);
  end

  final begin
    if (matched_real === 0) $fatal(1, "REAL=%%f did not match +REAL=8.25");
    if (rate != 8.25) $fatal(1, "rate was %f, expected 8.25", rate);
    $display("All checks passed");
  end
endmodule
