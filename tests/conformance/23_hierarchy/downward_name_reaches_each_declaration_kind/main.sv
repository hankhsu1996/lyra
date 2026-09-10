// LRM 23.8's Syntax 23-8 lists what a hierarchical path may end at -- a
// variable, a net, a parameter, a port, a named block, a function, a task --
// and LRM 23.6 states what a name may do with one: be read, be written, or be
// triggered off. A downward path reaches each declaration kind the same way,
// so one route serves every kind rather than each kind carrying its own.
// A static declared inside a named block is on the path too (LRM 23.9), and a
// class handle's property is reached through the handle the path lands on
// (LRM 8.4). A static of a subroutine body is the same rule and is its own
// case, because it is the one kind this path does not yet reach.
interface Bus;
  int shared = 3;
endinterface

module Child (
    input int taken
);
  parameter int Scaled = 7;
  typedef enum int {ELow = 1, EHigh = 9} mode_t;
  mode_t mode = EHigh;
  event stirred;
  int written = 0;

  class Cell;
    int held = 41;
  endclass
  Cell handle = new();

  initial begin : marked
    int kept = 55;
    #10;
  end

  always @(stirred) written = 1;
endmodule

module Top;
  int fed = 21;
  Child c (fed);

  int saw_parameter = 0;
  int saw_enum = 0;
  int saw_port = 0;
  int saw_block_static = 0;
  int saw_property = 0;

  // Read after the connection has settled: a port is an implied continuous
  // assignment (LRM 23.3.3), so reading one in the same time step it is first
  // driven states nothing about hierarchical names.
  initial begin
    #1;
    saw_parameter = c.Scaled;
    saw_enum = int'(c.mode);
    saw_port = c.taken;
    saw_block_static = c.marked.kept;
    saw_property = c.handle.held;
    -> c.stirred;
  end

  final begin
    if (saw_parameter !== 7)
      $fatal(1, "a parameter read %0d, expected 7", saw_parameter);
    if (saw_enum !== 9) $fatal(1, "an enum value read %0d, expected 9", saw_enum);
    if (saw_port !== 21) $fatal(1, "a port read %0d, expected 21", saw_port);
    if (saw_block_static !== 55)
      $fatal(1, "a named block's static read %0d, expected 55", saw_block_static);
    if (saw_property !== 41)
      $fatal(1, "a class property read %0d, expected 41", saw_property);
    if (c.written !== 1)
      $fatal(1, "triggering a named event left %0d, expected 1", c.written);
    $display("All checks passed");
  end
endmodule
