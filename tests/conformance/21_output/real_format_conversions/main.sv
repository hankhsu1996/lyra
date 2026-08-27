// The %f, %e and %g conversions take a real argument and have the full
// formatting capabilities of the C language. %f writes the value in decimal
// notation; %e writes it in exponential notation, with one digit before the
// point and an exponent carrying a sign and at least two digits; and %g
// writes whichever of the two is shorter, with the trailing zeros of the
// fraction dropped. Given no precision, %f and %e write six digits after the
// point and %g keeps six significant digits, and a precision written after a
// period replaces that count. A field width written before the period is a
// minimum: a result shorter than it is padded on the left and a longer one
// expands to fit (LRM 21.2.1.2, Table 21-2).
module Top;
  real pi;
  real hundred;
  real tiny;
  real negative;

  string decimal_default;
  string exponential_default;
  string general_default;
  string decimal_of_hundred;
  string exponential_of_hundred;
  string general_of_hundred;
  string exponential_of_tiny;
  string general_of_tiny;
  string decimal_of_negative;
  string decimal_precision;
  string exponential_precision;
  string decimal_in_field;

  initial begin
    pi = 3.14159265358979;
    hundred = 100.0;
    tiny = 0.00001;
    negative = -2.5;

    decimal_default = $sformatf("%f", pi);
    exponential_default = $sformatf("%e", pi);
    general_default = $sformatf("%g", pi);

    decimal_of_hundred = $sformatf("%f", hundred);
    exponential_of_hundred = $sformatf("%e", hundred);
    general_of_hundred = $sformatf("%g", hundred);

    exponential_of_tiny = $sformatf("%e", tiny);
    general_of_tiny = $sformatf("%g", tiny);

    decimal_of_negative = $sformatf("%f", negative);

    decimal_precision = $sformatf("%.3f", pi);
    exponential_precision = $sformatf("%.3e", pi);
    decimal_in_field = $sformatf("%10.3f", pi);
  end

  final begin
    if (decimal_default != "3.141593")
      $fatal(1, "%%f of pi was '%s', expected 3.141593", decimal_default);
    if (exponential_default != "3.141593e+00")
      $fatal(1, "%%e of pi was '%s', expected 3.141593e+00",
             exponential_default);
    if (general_default != "3.14159")
      $fatal(1, "%%g of pi was '%s', expected 3.14159", general_default);

    if (decimal_of_hundred != "100.000000")
      $fatal(1, "%%f of 100 was '%s', expected 100.000000",
             decimal_of_hundred);
    if (exponential_of_hundred != "1.000000e+02")
      $fatal(1, "%%e of 100 was '%s', expected 1.000000e+02",
             exponential_of_hundred);
    if (general_of_hundred != "100")
      $fatal(1, "%%g of 100 was '%s', expected 100", general_of_hundred);

    if (exponential_of_tiny != "1.000000e-05")
      $fatal(1, "%%e of 0.00001 was '%s', expected 1.000000e-05",
             exponential_of_tiny);
    if (general_of_tiny != "1e-05")
      $fatal(1, "%%g of 0.00001 was '%s', expected 1e-05", general_of_tiny);

    if (decimal_of_negative != "-2.500000")
      $fatal(1, "%%f of -2.5 was '%s', expected -2.500000",
             decimal_of_negative);

    if (decimal_precision != "3.142")
      $fatal(1, "%%.3f of pi was '%s', expected 3.142", decimal_precision);
    if (exponential_precision != "3.142e+00")
      $fatal(1, "%%.3e of pi was '%s', expected 3.142e+00",
             exponential_precision);
    if (decimal_in_field != "     3.142")
      $fatal(1, "%%10.3f of pi was '%s', expected 3.142 in a field of ten",
             decimal_in_field);
    $display("All checks passed");
  end
endmodule
