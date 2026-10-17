// A union is one storage read through any of its members (LRM 7.3), so
// assigning a whole union changes the variable whenever a read through some
// member would see a different value, and a process waiting on that member
// wakes (LRM 9.4.2) -- even where the bits that changed lie beyond what its
// first member covers. Assigning the same bits again changes nothing and wakes
// no one.
module Top;
  typedef union {
    logic [7:0] narrow;
    logic [31:0] wide;
  } overlay_t;

  overlay_t source;
  overlay_t copy;
  int wakes;

  initial begin
    #1;
    source.wide = 1;
    copy = source;
    #1;
    // Bits only the wider member covers.
    source.wide = 32'h0001_0001;
    copy = source;
    #1;
    // The same bits again.
    copy = source;
    #1;
  end

  initial begin
    forever begin
      @(copy.wide);
      wakes++;
    end
  end

  final begin
    if (copy.wide !== 32'h0001_0001)
      $fatal(1, "copy.wide was %h, expected 00010001", copy.wide);
    if (wakes !== 2) $fatal(1, "wakes was %0d, expected 2", wakes);
    $display("All checks passed");
  end
endmodule
