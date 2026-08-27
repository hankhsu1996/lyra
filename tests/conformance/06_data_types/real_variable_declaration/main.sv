// A real or shortreal variable declared with no initializer holds 0.0 when
// simulation starts, and an initializer on such a declaration takes effect
// before any initial or always procedure is started. A realtime declaration is
// synonymous with a real one, and a real variable declared inside a procedural
// block carries the same meaning as one declared at module level (LRM 6.8,
// Table 6-7, 6.12).
module Top;
  real no_init;
  shortreal short_no_init;
  realtime time_no_init;
  real with_init = 3.14;
  shortreal short_with_init = 2.5;
  realtime time_with_init = 1.25;
  real seen_by_procedure;
  real from_block;

  initial begin
    real local_r;
    seen_by_procedure = with_init;
    local_r = 0.5;
    from_block = local_r;
  end

  final begin
    if (no_init != 0.0)
      $fatal(1, "no_init was %g, expected 0.0", no_init);
    if (short_no_init != 0.0)
      $fatal(1, "short_no_init was %g, expected 0.0", short_no_init);
    if (time_no_init != 0.0)
      $fatal(1, "time_no_init was %g, expected 0.0", time_no_init);
    if (with_init != 3.14)
      $fatal(1, "with_init was %g, expected 3.14", with_init);
    if (short_with_init != 2.5)
      $fatal(1, "short_with_init was %g, expected 2.5", short_with_init);
    if (time_with_init != 1.25)
      $fatal(1, "time_with_init was %g, expected 1.25", time_with_init);
    if (seen_by_procedure != 3.14)
      $fatal(1, "seen_by_procedure was %g, expected 3.14",
             seen_by_procedure);
    if (from_block != 0.5)
      $fatal(1, "from_block was %g, expected 0.5", from_block);
    $display("All checks passed");
  end
endmodule
