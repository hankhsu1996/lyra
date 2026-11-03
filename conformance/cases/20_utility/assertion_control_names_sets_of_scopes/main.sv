// An assertion control task's list names scopes (LRM 20.11), and a name may
// stand for several: an array of instances is every instance of it (LRM
// 23.3.2), and a loop generate construct's name is every block the loop built
// (LRM 27.4), which an object running the loop with another count builds
// another number of. A block of a loop is named by its select, with a path or
// without one. A clocking block holds no assertion (LRM 14.3), so naming one
// selects nothing, and a list whose every name selects nothing is no call
// naming the whole design.
//
// Every assertion here holds, so each check of one runs its pass statement
// once, and a count of those is how many times it was checked.
module Leaf (
    input logic go
);
  int checked = 0;
  always @(go) assert (1) checked = checked + 1;
endmodule

module Mid #(
    parameter int N = 1
) (
    input logic go
);
  int checked = 0;
  for (genvar i = 0; i < N; i++) begin : g
    always @(go) assert (1) checked = checked + 1;
  end
endmodule

module Top;
  logic go = 0;
  logic clk = 0;
  int own = 0;

  clocking cb @(posedge clk);
  endclocking

  Leaf arr[3] (.go);
  Mid #(.N(2)) two (.go);
  Mid #(.N(4)) four (.go);
  for (genvar i = 0; i < 3; i++) begin : g
    always @(go) assert (1) own = own + 1;
  end

  int last[6];
  function automatic void Count(
      input int want_arr0, input int want_arr2, input int want_two,
      input int want_four, input int want_own, input string step);
    int now[6];
    int want[6];
    now = '{arr[0].checked, arr[2].checked, two.checked, four.checked, own, 0};
    want = '{want_arr0, want_arr2, want_two, want_four, want_own, 0};
    for (int i = 0; i < 6; i++) begin
      if (now[i] - last[i] !== want[i]) begin
        $fatal(1, "%s: count %0d moved by %0d, expected %0d", step, i,
               now[i] - last[i], want[i]);
      end
    end
    last = now;
  endfunction

  initial begin
    #1 go = ~go;
    #1 Count(1, 1, 2, 4, 3, "every assertion on");

    $assertoff(0, arr);
    #1 go = ~go;
    #1 Count(0, 0, 2, 4, 3, "every instance of an array off");

    $asserton;
    $assertoff(0, four.g);
    #1 go = ~go;
    #1 Count(1, 1, 2, 0, 3, "every block of another instance's loop off");

    $asserton;
    $assertoff(0, g);
    #1 go = ~go;
    #1 Count(1, 1, 2, 4, 0, "every block of this scope's loop off");

    $asserton;
    $assertoff(0, g[1]);
    #1 go = ~go;
    #1 Count(1, 1, 2, 4, 2, "one block named without a path off");

    $asserton;
    $assertoff(0, two.g[0]);
    #1 go = ~go;
    #1 Count(1, 1, 1, 4, 3, "one block named with a path off");

    $asserton;
    $assertoff(0, cb);
    #1 go = ~go;
    #1 Count(1, 1, 2, 4, 3, "a clocking block selects nothing");

    $display("All checks passed");
  end
endmodule
