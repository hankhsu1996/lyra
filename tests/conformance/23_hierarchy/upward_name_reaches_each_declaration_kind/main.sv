// The same declaration kinds LRM 23.8 lists, reached the other way: a child
// naming an enclosing module (LRM 23.8) rather than a parent naming a child.
// Direction is not what decides whether a kind is reachable -- one route
// serves both -- so this case asserts of an upward path exactly what its
// downward sibling asserts of a downward one.
module Child;
  int saw_parameter = 0;
  int saw_enum = 0;
  int saw_block_static = 0;

  initial begin
    #1;
    saw_parameter = Top.Scaled;
    saw_enum = int'(Top.mode);
    saw_block_static = Top.marked.kept;
    -> Top.stirred;
  end
endmodule

module Top;
  parameter int Scaled = 7;
  typedef enum int {ELow = 1, EHigh = 9} mode_t;
  mode_t mode = EHigh;
  event stirred;
  int written = 0;

  initial begin : marked
    int kept = 55;
    #10;
  end

  always @(stirred) written = 1;

  Child c ();

  final begin
    if (c.saw_parameter !== 7)
      $fatal(1, "a parameter read %0d, expected 7", c.saw_parameter);
    if (c.saw_enum !== 9)
      $fatal(1, "an enum value read %0d, expected 9", c.saw_enum);
    if (c.saw_block_static !== 55)
      $fatal(
          1, "a named block's static read %0d, expected 55",
          c.saw_block_static);
    if (written !== 1)
      $fatal(1, "triggering a named event left %0d, expected 1", written);
    $display("All checks passed");
  end
endmodule
