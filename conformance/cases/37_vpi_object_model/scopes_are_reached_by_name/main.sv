// `vpi_handle_by_name` reaches a scope by the full name it answers, or by a
// name relative to a scope it is handed and only within it (LRM 38.21): an
// instance, an element of an instance array and of a loop generate, an
// unnamed generate block by the name LRM 27.6 gives it, a named block of a
// process or of a task, one inside a block the source gave no name from that
// block alone (LRM 9.3.4), an escaped identifier, and a task a package
// declares. A name that names nothing is answered with no handle, and so is
// one naming what the compilation unit's scope declares or what an import
// makes visible (LRM 37.10 detail 6). Each
// scope's full name is the hierarchical name `%m` prints inside it (LRM
// 21.2.1.5, 37.10), so the design hands what `%m` printed and the foreign half
// asks for the full name of what it reaches.
package P;
  import "DPI-C" context function void keep(string written, string name);
  task automatic pt();
    keep($sformatf("%m"), "P::pt");
  endtask
endpackage

task automatic unit_task();
endtask

module Leaf;
  import "DPI-C" context function void keep(string written, string name);
  initial keep($sformatf("%m"), "");
endmodule

module Top;
  import P::*;
  import "DPI-C" context function void keep(string written, string name);
  import "DPI-C" context function int look();

  Leaf u[1:0] ();
  Leaf \esc.name ();
  for (genvar i = 0; i < 2; i++) begin : g
    Leaf l ();
  end
  if (1) begin
    Leaf in_unnamed ();
  end

  task t;
    begin : in_task
      keep($sformatf("%m"), "Top.t.in_task");
    end
  endtask

  int differences = -1;

  initial begin
    int x;
    begin : deep
    end
  end

  initial begin : main
    begin : inner
      keep($sformatf("%m"), "Top.main.inner");
    end
    t();
    P::pt();
    #1 differences = look();
  end

  final begin
    if (differences !== 0)
      $fatal(1, "the lookup found %0d differences", differences);
    $display("All checks passed");
  end
endmodule
