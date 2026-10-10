// A named object is referenced uniquely in its full form by concatenating the
// names of the scopes that contain it, a period separating each from the
// next, except that an escaped identifier in such a name is followed by white
// space and then the period (LRM 23.6). The backslash and the white space are
// no part of the identifier, so an escaped identifier spelling a simple one is
// that simple identifier and is written without either, while an escaped
// keyword is an identifier only as long as it is escaped (LRM 5.6.1). A name
// referring to an element of an instance array is followed by an instance
// select, which is one of the legal index values of the array, and so one of
// the values its range declares (LRM 23.6, 28.3.5).
//
// So the hierarchical name a scope reports for itself (LRM 21.2.1.5) is text
// that refers to that scope and to no other: two scopes whose identifiers
// spell alike once joined by periods report different names, and an element
// of an array reports the index it is selected by.
module Leaf;
  string name = "unset";
  initial name = $sformatf("%m");
endmodule

module \Kind+1 ;
  string name = "unset";
  initial name = $sformatf("%m");
endmodule

module Top;
  Leaf \inst.x ();
  Leaf \cpu3 ();
  Leaf \begin ();
  \Kind+1 plain ();

  Leaf rising [3:5] ();
  Leaf falling [5:3] ();
  Leaf grid [1:2][7:6] ();

  if (1) begin : a
    if (1) begin : b
      string name = "unset";
      initial name = $sformatf("%m");
    end
  end
  if (1) begin : \a.b
    string name = "unset";
    initial name = $sformatf("%m");
  end

  for (genvar i = 3; i < 5; i++) begin : \loop[0]
    string name = "unset";
    initial name = $sformatf("%m");
  end

  string in_task = "unset";
  string in_function = "unset";
  string in_block = "unset";
  string in_nested = "unset";

  task automatic \t-1 ();
    in_task = $sformatf("%m");
  endtask

  function automatic void \f.g ();
    in_function = $sformatf("%m");
  endfunction

  initial begin : \blk.1
    in_block = $sformatf("%m");
    begin : inner
      in_nested = $sformatf("%m");
    end
    \t-1 ();
    \f.g ();
  end

  final begin
    if (\inst.x .name != "Top.\\inst.x ")
      $fatal(1, "the escaped instance reported '%s'", \inst.x .name);
    if (cpu3.name != "Top.cpu3")
      $fatal(1, "the escaped simple identifier reported '%s'", cpu3.name);
    if (\begin .name != "Top.\\begin ")
      $fatal(1, "the escaped keyword reported '%s'", \begin .name);
    if (plain.name != "Top.plain")
      $fatal(1, "the instance of an escaped module reported '%s'", plain.name);

    if (rising[3].name != "Top.rising[3]")
      $fatal(1, "rising[3] reported '%s'", rising[3].name);
    if (rising[5].name != "Top.rising[5]")
      $fatal(1, "rising[5] reported '%s'", rising[5].name);
    if (falling[5].name != "Top.falling[5]")
      $fatal(1, "falling[5] reported '%s'", falling[5].name);
    if (falling[3].name != "Top.falling[3]")
      $fatal(1, "falling[3] reported '%s'", falling[3].name);
    if (grid[1][7].name != "Top.grid[1][7]")
      $fatal(1, "grid[1][7] reported '%s'", grid[1][7].name);
    if (grid[2][6].name != "Top.grid[2][6]")
      $fatal(1, "grid[2][6] reported '%s'", grid[2][6].name);

    if (a.b.name != "Top.a.b")
      $fatal(1, "the block in a block reported '%s'", a.b.name);
    if (\a.b .name != "Top.\\a.b ")
      $fatal(1, "the escaped block reported '%s'", \a.b .name);
    if (\loop[0] [3].name != "Top.\\loop[0] [3]")
      $fatal(1, "the escaped loop's block 3 reported '%s'", \loop[0] [3].name);
    if (\loop[0] [4].name != "Top.\\loop[0] [4]")
      $fatal(1, "the escaped loop's block 4 reported '%s'", \loop[0] [4].name);

    if (in_block != "Top.\\blk.1 ")
      $fatal(1, "the escaped named block reported '%s'", in_block);
    if (in_nested != "Top.\\blk.1 .inner")
      $fatal(1, "the block in the escaped block reported '%s'", in_nested);
    if (in_task != "Top.\\t-1 ")
      $fatal(1, "the escaped task reported '%s'", in_task);
    if (in_function != "Top.\\f.g ")
      $fatal(1, "the escaped function reported '%s'", in_function);
    $display("All checks passed");
  end
endmodule
