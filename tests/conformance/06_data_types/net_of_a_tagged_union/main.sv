// The data type of a net may be an unpacked union whose every member is itself
// a valid data type for a net (LRM 6.7.1), and the clause makes no exception
// for a tagged one (LRM 7.3.2). A net of such a type with one driver carries
// the value that driver gives it: the member the driver named, read through
// that member, and the tag naming it.
typedef union tagged {
  logic [7:0] wide;
  logic [3:0] narrow;
} slot_t;

module Top;
  wire slot_t held;

  assign held = tagged wide 8'h5a;

  logic [7:0] seen_wide;
  bit holds_wide;

  initial begin
    #1;
    holds_wide = 1'b0;
    if (held matches tagged wide .*) holds_wide = 1'b1;
    seen_wide = held.wide;
  end

  final begin
    if (holds_wide !== 1'b1)
      $fatal(1, "a tagged union net did not hold the member its driver named");
    if (seen_wide !== 8'h5a)
      $fatal(1, "a tagged union net's member read %h, expected 5a", seen_wide);
    $display("All checks passed");
  end
endmodule
