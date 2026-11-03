// The assertion control tasks act on the assertions a call selects (LRM
// 20.11). With no list a call reaches every assertion; a list names
// instances, blocks, tasks and assertions, and `levels` says how many levels of
// instances below a named instance it reaches, as `$dumpvars` counts them
// (LRM 21.7.1.2): 0 is every level, and 1 is the instance's own assertions,
// those of its blocks included. A statement label puts a named block around
// the statement it labels (LRM 16.3), so an assertion is named as that block
// is. `$assertcontrol` selects by type and directive as well (Tables 20-6,
// 20-7), and Lock keeps what it locks from every change until Unlock.
//
// Every assertion here holds, so each check of one runs its pass statement
// once, and a count of those is how many times it was checked. An assertion
// turned off is not checked at all, and runs no statement.
module Leaf (
    input logic go
);
  int checked = 0;
  always @(go) begin
    check : assert (1) checked = checked + 1;
  end
endmodule

module Mid (
    input logic go
);
  int checked = 0;
  int generated = 0;
  Leaf leaf (.go);
  always @(go) assert (1) checked = checked + 1;
  if (1) begin : g
    always @(go) assert (1) generated = generated + 1;
  end
endmodule

module Top;
  logic go = 0;
  int named_checked = 0;
  int covered = 0;
  Mid u0 (.go);
  Mid u1 (.go);

  always @(go) begin : named
    assert (1) named_checked = named_checked + 1;
  end
  always @(go) cover (1) covered = covered + 1;

  int tasked = 0;
  task Check;
    assert (1) tasked = tasked + 1;
  endtask
  always @(go) Check();

  // How many checks each assertion has seen since `Count` was last asked.
  int last[9];
  function automatic void Count(
      input int want_named, input int want_u0, input int want_u0_g,
      input int want_u0_leaf, input int want_u1, input int want_u1_g,
      input int want_u1_leaf, input int want_cover, input int want_task,
      input string step);
    int now[9];
    int want[9];
    now = '{named_checked, u0.checked, u0.generated, u0.leaf.checked,
            u1.checked, u1.generated, u1.leaf.checked, covered, tasked};
    want = '{want_named, want_u0, want_u0_g, want_u0_leaf, want_u1, want_u1_g,
             want_u1_leaf, want_cover, want_task};
    for (int i = 0; i < 9; i++) begin
      if (now[i] - last[i] !== want[i]) begin
        $fatal(1, "%s: assertion %0d was checked %0d times, expected %0d",
               step, i, now[i] - last[i], want[i]);
      end
    end
    last = now;
  endfunction

  initial begin
    #1 go = ~go;
    #1 Count(1, 1, 1, 1, 1, 1, 1, 1, 1, "every assertion on");

    $assertoff(0, u0);
    #1 go = ~go;
    #1 Count(1, 0, 0, 0, 1, 1, 1, 1, 1, "u0 and below off");

    $asserton(0, u0);
    $assertoff(1, u1);
    #1 go = ~go;
    #1 Count(1, 1, 1, 1, 0, 0, 1, 1, 1, "u1's own level off");

    $asserton;
    $assertoff(0, named);
    #1 go = ~go;
    #1 Count(0, 1, 1, 1, 1, 1, 1, 1, 1, "a named block off");

    $asserton;
    $assertoff(0, u1.leaf.check);
    #1 go = ~go;
    #1 Count(1, 1, 1, 1, 1, 1, 0, 1, 1, "a labelled assertion off");

    $asserton;
    $assertoff;
    #1 go = ~go;
    #1 Count(0, 0, 0, 0, 0, 0, 0, 0, 0, "everything off");

    $asserton(0);
    // Concurrent assertions only, which none of these is.
    $assertcontrol(4, 1);
    #1 go = ~go;
    #1 Count(1, 1, 1, 1, 1, 1, 1, 1, 1, "concurrent assertions off");

    // Simple immediate cover directives only.
    $assertcontrol(4, 2, 2);
    #1 go = ~go;
    #1 Count(1, 1, 1, 1, 1, 1, 1, 0, 1, "covers off");

    $assertcontrol(3);
    $assertcontrol(1, 15, 7, 0, u0);
    $assertoff(0, u0);
    #1 go = ~go;
    #1 Count(1, 1, 1, 1, 1, 1, 1, 1, 1, "u0 locked against off");

    $assertcontrol(2, 15, 7, 0, u0);
    $assertoff(0, u0);
    #1 go = ~go;
    #1 Count(1, 0, 0, 0, 1, 1, 1, 1, 1, "u0 unlocked and off");

    $asserton;
    $assertoff(0, Check);
    #1 go = ~go;
    #1 Count(1, 1, 1, 1, 1, 1, 1, 1, 0, "a task off");

    $display("All checks passed");
  end
endmodule
