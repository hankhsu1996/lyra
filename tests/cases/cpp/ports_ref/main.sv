// A `ref` port aliases the connected variable: the child's port and the
// parent's variable are one storage cell, not two cells joined by a continuous
// assignment (LRM 23.3.3.2). A child write is immediately the parent's value
// (zero delta), a child read sees the parent's current value, and a child write
// fires the connected signal's update event so a parent process sensitive to it
// wakes.
module Child(ref int r);
  initial begin
    #2;
    r = r + 100;
  end
endmodule

module Top;
  int shared;
  int mirror;
  Child c(.r(shared));
  always @(shared) mirror = shared;
  initial begin
    shared = 5;
    #1 shared = 9;
  end
  final $display("shared=%0d mirror=%0d", shared, mirror);
endmodule
