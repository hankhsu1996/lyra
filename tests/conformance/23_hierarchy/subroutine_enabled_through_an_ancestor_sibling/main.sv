// LRM 23.8 step b resolves a name against the children of each enclosing
// scope, so a subroutine is enabled on a sibling of an ancestor -- neither
// above the caller nor below it -- and on an absolute path from `$root`
// (LRM 23.6). Both reach the same instance, so both leave the same state
// changed.
module Peer;
  int count = 0;

  task automatic Bump(input int by);
    #1;
    count = count + by;
  endtask

  function automatic int Doubled();
    return count * 2;
  endfunction
endmodule

module Caller;
  int through_sibling = 0;
  int through_root = 0;

  initial begin
    Top.p.Bump(3);
    through_sibling = Top.p.Doubled();
    through_root = $root.Top.p.Doubled();
  end
endmodule

module Top;
  Peer p ();
  Caller u ();

  final begin
    if (p.count !== 3) $fatal(1, "count was %0d, expected 3", p.count);
    if (u.through_sibling !== 6)
      $fatal(1, "a sibling of an ancestor read %0d, expected 6",
             u.through_sibling);
    if (u.through_root !== 6)
      $fatal(1, "an absolute path read %0d, expected 6", u.through_root);
    $display("All checks passed");
  end
endmodule
