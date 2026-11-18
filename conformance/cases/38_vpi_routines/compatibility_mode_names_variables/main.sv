// What a variable or an array of them is, and what a scope's relations to them
// hold, are what the version an application was compiled for says (LRM 36.12.1
// Table 36-10, 36.12.2.1). Under 1364-1995 a one-dimensional array of `reg` is
// a memory and each element a memory word; under every version of 1364 an array
// of `integer` is an integer variable, one of `real` is a real variable from
// 1364-2001 on, and a scope's variables leave out its `reg`s and arrays of
// them, which its `reg` relation and, under 1364-1995, its memory relation and,
// from 1364-2001 on, its `reg` array relation hold, the last no array of any
// other type (LRM 36.12.1 entry 7). From 1800-2005 on, every array is an array
// variable and every `reg` a logic variable. A scope's variables hold them all,
// its `reg` array relation every array, and its memory relation still a
// one-dimensional array of `reg`. Each foreign source here is compiled for one
// mode and asks what that mode answers. An application of a 1364 mode is to be
// applied to a partition of the design holding only constructs that mode has
// (LRM 36.12.3), so the variables it asks about are those of an instance of a
// module written in 1364 alone.
module Legacy;
  reg [7:0] mem[0:3];
  reg r1;
  integer ia[0:1];
  real ra[0:1];
  integer single;
endmodule

module Top;
  import "DPI-C" context function int look_in_1364v1995_mode();
  import "DPI-C" context function int look_in_1364v2005_mode();
  import "DPI-C" context function int look_in_1800v2012_mode();

  Legacy legacy ();
  int counts[0:3];

  int differences = -1;

  initial
    #1 differences = look_in_1364v1995_mode() + look_in_1364v2005_mode() +
        look_in_1800v2012_mode();

  final begin
    if (differences !== 0)
      $fatal(1, "the modes differ in %0d answers", differences);
    $display("All checks passed");
  end
endmodule
