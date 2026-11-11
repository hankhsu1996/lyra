// Every kind of scope the design declares is a VPI object reached from the
// one above it (LRM 37.5 to 37.12, 37.85): the top-level module from no object
// (37.5), packages among the instances (37.10), module, interface and program
// instances and the arrays of them (37.11) by their own relations, generate
// scopes and the set a loop generate builds, named or not (37.85), and the
// procedural scopes -- named blocks, unnamed ones declaring a variable or a
// type, forks by how each joins, `for` and `foreach` loops, tasks, functions,
// classes and clocking blocks (37.12) -- through `vpiInternalScope`. An
// instance's tasks, functions and classes are its own relations too, a class
// is one class defn however many of its specializations the design uses
// (37.31), and a label on a `for` declaring no variable names a block of its
// own (9.3.5); a function imported from C is one of its scope's functions
// (37.41 detail 6). Each object answers the module and the instance it lies in
// (37.10 details 2, 3). Two instances of one module are two objects with
// relations of their own (36.10): here a parameter changes how many blocks a
// loop builds and whether an arm is built, and one task declared in a loop's
// block is a task of each block it built. An iterator answers the object it
// was made on (37.84), and two handles to one object compare equal while each
// is released on its own (37.2, 38.3, 38.38). The foreign half states what it
// expects and counts what differs.
package P;
  task automatic pt();
  endtask
endpackage

interface Bus;
endinterface

program Prog;
endprogram

module Leaf #(parameter int N = 1);
  if (N > 1) begin : wide
  end
  for (genvar i = 0; i < N; i++) begin : g
  end
endmodule

module Top;
  import "DPI-C" context function int walk();

  Leaf #(1) one ();
  Leaf #(3) three ();
  Leaf grid[1:0][2:0] ();
  Bus buses[2] ();
  Bus bus ();
  Prog prog ();
  if (1) begin
    Leaf in_unnamed ();
  end
  for (genvar k = 0; k < 0; k++) begin : none
  end
  for (genvar j = 0; j < 2; j++) begin : tg
    task t;
    endtask
  end
  for (genvar m = 0; m < 1; m++) begin
  end

  task t;
    begin : in_task
    end
  endtask
  function void f;
  endfunction
  class C;
  endclass
  class G #(int W = 1);
  endclass
  G #(1) g1;
  G #(2) g2;
  logic clk;
  clocking cb @(posedge clk);
  endclocking

  int differences = -1;
  int pair[2];
  int count;

  initial begin : main
    begin : inner
      int x;
    end
    fork : fa
      begin
      end
    join_any
    fork : fj
    join
    fork : fn
    join_none
    begin
      int unnamed;
    end
    begin
      typedef logic [3:0] nibble_t;
    end
    lbl: for (count = 0; count < 1; count++) begin
    end
    for (int i = 0; i < 1; i++) begin
    end
    foreach (pair[k]) begin
    end
    differences = walk();
  end

  final begin
    if (differences !== 0)
      $fatal(1, "the walk found %0d differences", differences);
    $display("All checks passed");
  end
endmodule
