// The same declaration kinds LRM 23.8 lists, reached the other way: a child
// naming an enclosing module (LRM 23.8) rather than a parent naming a child.
// Direction is not what decides whether a kind is reachable -- one route
// serves both -- so this case asserts of an upward path exactly what its
// downward sibling asserts of a downward one. A port is among those kinds, and
// reaching one upward takes a module between the top and the reader, since a
// top has nothing to connect its own ports to.
module Child;
  int saw_parameter = 0;
  int saw_enum = 0;
  int saw_block_static = 0;
  int saw_port = 0;

  initial begin
    #1;
    saw_parameter = Top.Scaled;
    saw_enum = int'(Top.mode);
    saw_block_static = Top.marked.kept;
    saw_port = Mid.fed;
    ->Top.stirred;
  end
endmodule

module Mid (
    input int fed
);
  Child c ();
endmodule

module Top;
  parameter int Scaled = 7;
  typedef enum int {
    ELow  = 1,
    EHigh = 9
  } mode_t;
  mode_t mode = EHigh;
  event stirred;
  int written = 0;
  int drive = 21;

  initial begin : marked
    int kept = 55;
    #10;
  end

  always @(stirred) written = 1;

  Mid m (.fed(drive));

  final begin
    if (m.c.saw_parameter !== 7)
      $fatal(1, "a parameter read %0d, expected 7", m.c.saw_parameter);
    if (m.c.saw_enum !== 9)
      $fatal(1, "an enum value read %0d, expected 9", m.c.saw_enum);
    if (m.c.saw_block_static !== 55)
      $fatal(
          1, "a named block's static read %0d, expected 55",
          m.c.saw_block_static);
    if (m.c.saw_port !== 21)
      $fatal(1, "a port read %0d, expected 21", m.c.saw_port);
    if (written !== 1)
      $fatal(1, "triggering a named event left %0d, expected 1", written);
    $display("All checks passed");
  end
endmodule
