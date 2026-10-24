// @error: cannot schedule what takes effect after it returns
//
// A function may hold a fork-join_none, but scheduling what cannot become
// active until the function returns is allowed only on a thread an initial or
// always procedure created, or a fork of one, and a tool issues an error
// otherwise (LRM 13.4.4). The final procedures are function calls from a
// process neither created (LRM 9.2.3), so the fork is refused when it runs.
module Top;
  initial #5;

  final begin
    fork
      $display("a process spawned in a final procedure");
    join_none
  end
endmodule
