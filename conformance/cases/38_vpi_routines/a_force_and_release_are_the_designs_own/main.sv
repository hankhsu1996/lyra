// `vpi_put_value` with `vpiForceFlag` is a procedural force of the variable
// (LRM 38.34, 10.6.2): it shows the value forced, what waits on it wakes, and
// a procedural assignment while it is forced changes nothing it shows. With
// `vpiReleaseFlag` it is the release, after which the variable keeps the value
// it was forced to until it is next assigned, and the value it holds after is
// handed back. A procedural force takes no bit-select of a variable, nor a
// member or an element of one (LRM 10.6.2), and neither does foreign code's;
// nor does it take a variable declared `const`, which is not written (LRM
// 6.20.6).
module Top;
  import "DPI-C" context function int force_it();
  import "DPI-C" context function int release_it(output int after);
  import "DPI-C" context function int force_a_part();

  typedef struct packed {
    bit [3:0] hi;
    bit [3:0] lo;
  } nibbles_t;

  logic [7:0] f = 8'h01;
  nibbles_t ps = 8'h35;
  int arr[4] = '{1, 2, 3, 4};
  const int fixed = 4;
  int woke = 0;
  int after = -1;
  int differences = -1;

  always @(f) woke++;

  initial begin
    #1 differences = force_it();
    #1;
    if (f !== 8'h55) $fatal(1, "the forced f shows %h, expected 55", f);
    if (woke !== 1) $fatal(1, "a wait on f woke %0d times at the force", woke);
    f = 8'h11;
    #1;
    if (f !== 8'h55) $fatal(1, "an assignment changed the forced f to %h", f);
    differences += release_it(after);
    if (after !== 8'h55) $fatal(1, "the release handed back %h", after);
    #1;
    if (f !== 8'h55) $fatal(1, "the released f shows %h, expected 55", f);
    f = 8'h22;
    #1;
    if (f !== 8'h22) $fatal(1, "an assignment after release left %h", f);
    differences += force_a_part();
    if (ps !== 8'h35) $fatal(1, "a part force changed ps to %h", ps);
    if (arr[2] !== 3)
      $fatal(1, "an element force changed arr[2] to %0d", arr[2]);
    if (fixed !== 4) $fatal(1, "a force changed the const fixed to %0d", fixed);
  end

  final begin
    if (differences !== 0)
      $fatal(1, "the routines differ in %0d answers", differences);
    $display("All checks passed");
  end
endmodule
