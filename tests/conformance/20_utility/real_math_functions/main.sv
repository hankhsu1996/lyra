// The real math functions accept real arguments and return a real result, and
// each one behaves as the C standard math library function it is cross-listed
// with (LRM 20.8.2). How accurate that function is belongs to the C library and
// not to this table, so nothing here measures accuracy; what a case can hold an
// implementation to is that every name reaches the operation the table gives
// it. Two kinds of claim do that between them. The exact ones are values
// floating-point arithmetic lands on with nothing to round away, and they also
// tell each function of a pair apart from the other -- sine from cosine at
// zero, floor from ceiling below a half. The rest are identities that are true
// of the mathematics itself, so no decimal is written down for a reader to take
// on trust; each is checked to a margin far above what rounding a few
// operations can cost and far below what naming the wrong operation would.
module Top;
  parameter real Quarter = 0.25;
  localparam real Tolerance = 1e-12;

  real zero = 0.0;
  real half = 0.5;
  real one = 1.0;
  real two = 2.0;
  real ten = 10.0;
  real nine = 9.0;
  real three = 3.0;
  real four = 4.0;
  real seven = 7.0;
  real neg_half_step = -2.5;

  real folded_sqrt = -1.0;

  real r_sqrt = -1.0;
  real r_pow = -1.0;
  real r_floor = -1.0;
  real r_ceil = -1.0;
  real r_hypot = -1.0;
  real r_exp_zero = -1.0;
  real r_sin_zero = -1.0;
  real r_cos_zero = -1.0;
  real r_sinh_zero = -1.0;
  real r_cosh_zero = -1.0;

  real d_ln_undoes_exp = 1.0;
  real d_log10_scales_ln = 1.0;
  real d_pythagorean = 1.0;
  real d_tan_is_sin_over_cos = 1.0;
  real d_asin_undoes_sin = 1.0;
  real d_acos_undoes_cos = 1.0;
  real d_atan_undoes_tan = 1.0;
  real d_atan2_matches_atan = 1.0;
  real d_hyperbolic_unit = 1.0;
  real d_sinh_from_exp = 1.0;
  real d_tanh_is_sinh_over_cosh = 1.0;
  real d_asinh_undoes_sinh = 1.0;
  real d_acosh_undoes_cosh = 1.0;
  real d_atanh_undoes_tanh = 1.0;

  initial begin
    folded_sqrt = $sqrt(Quarter);

    r_sqrt = $sqrt(nine);
    r_pow = $pow(two, ten);
    r_floor = $floor(neg_half_step);
    r_ceil = $ceil(neg_half_step);
    r_hypot = $hypot(three, four);
    r_exp_zero = $exp(zero);
    r_sin_zero = $sin(zero);
    r_cos_zero = $cos(zero);
    r_sinh_zero = $sinh(zero);
    r_cosh_zero = $cosh(zero);

    d_ln_undoes_exp = $ln($exp(two)) - two;
    d_log10_scales_ln = ($log10(seven) * $ln(ten)) - $ln(seven);
    d_pythagorean = ($sin(half) * $sin(half)) + ($cos(half) * $cos(half)) - one;
    d_tan_is_sin_over_cos = $tan(half) - ($sin(half) / $cos(half));
    d_asin_undoes_sin = $asin($sin(half)) - half;
    d_acos_undoes_cos = $acos($cos(half)) - half;
    d_atan_undoes_tan = $atan($tan(half)) - half;
    d_atan2_matches_atan = $atan2(one, two) - $atan(one / two);
    d_hyperbolic_unit =
        ($cosh(half) * $cosh(half)) - ($sinh(half) * $sinh(half)) - one;
    d_sinh_from_exp = $sinh(half) - (($exp(half) - $exp(-half)) / two);
    d_tanh_is_sinh_over_cosh = $tanh(half) - ($sinh(half) / $cosh(half));
    d_asinh_undoes_sinh = $asinh($sinh(half)) - half;
    d_acosh_undoes_cosh = $acosh($cosh(half)) - half;
    d_atanh_undoes_tanh = $atanh($tanh(half)) - half;
  end

  final begin
    if (folded_sqrt != 0.5)
      $fatal(1, "$sqrt of a constant 0.25 was %g, expected 0.5", folded_sqrt);
    if (r_sqrt != 3.0) $fatal(1, "$sqrt(9.0) was %g, expected 3.0", r_sqrt);
    if (r_pow != 1024.0)
      $fatal(1, "$pow(2.0, 10.0) was %g, expected 1024.0", r_pow);
    if (r_floor != -3.0)
      $fatal(1, "$floor(-2.5) was %g, expected -3.0", r_floor);
    if (r_ceil != -2.0)
      $fatal(1, "$ceil(-2.5) was %g, expected -2.0", r_ceil);
    if (r_hypot != 5.0)
      $fatal(1, "$hypot(3.0, 4.0) was %g, expected 5.0", r_hypot);
    if (r_exp_zero != 1.0)
      $fatal(1, "$exp(0.0) was %g, expected 1.0", r_exp_zero);
    if (r_sin_zero != 0.0)
      $fatal(1, "$sin(0.0) was %g, expected 0.0", r_sin_zero);
    if (r_cos_zero != 1.0)
      $fatal(1, "$cos(0.0) was %g, expected 1.0", r_cos_zero);
    if (r_sinh_zero != 0.0)
      $fatal(1, "$sinh(0.0) was %g, expected 0.0", r_sinh_zero);
    if (r_cosh_zero != 1.0)
      $fatal(1, "$cosh(0.0) was %g, expected 1.0", r_cosh_zero);

    if (d_ln_undoes_exp > Tolerance || d_ln_undoes_exp < -Tolerance)
      $fatal(1, "$ln($exp(x)) missed x by %g", d_ln_undoes_exp);
    if (d_log10_scales_ln > Tolerance || d_log10_scales_ln < -Tolerance)
      $fatal(1, "$log10(x) * $ln(10) missed $ln(x) by %g", d_log10_scales_ln);
    if (d_pythagorean > Tolerance || d_pythagorean < -Tolerance)
      $fatal(1, "$sin(x)^2 + $cos(x)^2 missed 1 by %g", d_pythagorean);
    if (d_tan_is_sin_over_cos > Tolerance ||
        d_tan_is_sin_over_cos < -Tolerance)
      $fatal(1, "$tan(x) missed $sin(x)/$cos(x) by %g", d_tan_is_sin_over_cos);
    if (d_asin_undoes_sin > Tolerance || d_asin_undoes_sin < -Tolerance)
      $fatal(1, "$asin($sin(x)) missed x by %g", d_asin_undoes_sin);
    if (d_acos_undoes_cos > Tolerance || d_acos_undoes_cos < -Tolerance)
      $fatal(1, "$acos($cos(x)) missed x by %g", d_acos_undoes_cos);
    if (d_atan_undoes_tan > Tolerance || d_atan_undoes_tan < -Tolerance)
      $fatal(1, "$atan($tan(x)) missed x by %g", d_atan_undoes_tan);
    if (d_atan2_matches_atan > Tolerance || d_atan2_matches_atan < -Tolerance)
      $fatal(1, "$atan2(y, x) missed $atan(y/x) by %g", d_atan2_matches_atan);
    if (d_hyperbolic_unit > Tolerance || d_hyperbolic_unit < -Tolerance)
      $fatal(1, "$cosh(x)^2 - $sinh(x)^2 missed 1 by %g", d_hyperbolic_unit);
    if (d_sinh_from_exp > Tolerance || d_sinh_from_exp < -Tolerance)
      $fatal(1, "$sinh(x) missed its definition in $exp by %g",
             d_sinh_from_exp);
    if (d_tanh_is_sinh_over_cosh > Tolerance ||
        d_tanh_is_sinh_over_cosh < -Tolerance)
      $fatal(1, "$tanh(x) missed $sinh(x)/$cosh(x) by %g",
             d_tanh_is_sinh_over_cosh);
    if (d_asinh_undoes_sinh > Tolerance || d_asinh_undoes_sinh < -Tolerance)
      $fatal(1, "$asinh($sinh(x)) missed x by %g", d_asinh_undoes_sinh);
    if (d_acosh_undoes_cosh > Tolerance || d_acosh_undoes_cosh < -Tolerance)
      $fatal(1, "$acosh($cosh(x)) missed x by %g", d_acosh_undoes_cosh);
    if (d_atanh_undoes_tanh > Tolerance || d_atanh_undoes_tanh < -Tolerance)
      $fatal(1, "$atanh($tanh(x)) missed x by %g", d_atanh_undoes_tanh);

    $display("All checks passed");
  end
endmodule
