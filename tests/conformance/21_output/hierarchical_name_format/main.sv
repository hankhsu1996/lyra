// The %m format specification takes no argument. It prints the hierarchical
// name of the design element, subroutine, named block, or labeled statement
// that invokes the system task containing it, and a hierarchical name is the
// names of the module instances, tasks, functions, and named blocks that
// contain the invoker, joined by periods and rooted at the top-level module.
// So the name follows the scope the call sits in: an instance contributes its
// instance name, a subroutine and a named block each contribute their own,
// and a labeled statement contributes its label because the label names a
// block around the statement. An unnamed block is none of those and
// contributes nothing. A named generate block instance contributes its name
// as any other scope does, and where a loop generate construct made it, that
// name carries in square brackets the value the loop index held when the
// instance was elaborated (LRM 21.2.1.5, 23.6, 9.3.5, 27.5, 27.4).
module Leaf;
  string in_instance;
  string in_task;

  task automatic stamp();
    in_task = $sformatf("%m");
  endtask

  initial begin
    in_instance = $sformatf("%m");
    stamp();
  end
endmodule

module Top;
  string at_top;
  string in_named;
  string in_nested;
  string in_unnamed;
  string in_labeled;
  string in_function;

  Leaf leaf ();

  function automatic string stamp();
    return $sformatf("%m");
  endfunction

  initial at_top = $sformatf("%m");

  initial begin : outer
    in_named = $sformatf("%m");
    begin : inner
      in_nested = $sformatf("%m");
    end
    begin
      in_unnamed = $sformatf("%m");
    end
    tagged_stmt : in_labeled = $sformatf("%m");
    in_function = stamp();
  end

  if (1) begin : g_chosen
    string in_generate;
    initial in_generate = $sformatf("%m");
  end

  for (genvar i = 0; i < 2; i++) begin : g_repeated
    string in_generate;
    initial in_generate = $sformatf("%m");
  end

  final begin
    if (at_top != "Top")
      $fatal(1, "%%m in the module was %s, expected Top", at_top);
    if (leaf.in_instance != "Top.leaf")
      $fatal(1, "%%m in the instance was %s, expected Top.leaf",
             leaf.in_instance);
    if (leaf.in_task != "Top.leaf.stamp")
      $fatal(1, "%%m in the task was %s, expected Top.leaf.stamp",
             leaf.in_task);
    if (in_named != "Top.outer")
      $fatal(1, "%%m in the named block was %s, expected Top.outer", in_named);
    if (in_nested != "Top.outer.inner")
      $fatal(1, "%%m in the nested block was %s, expected Top.outer.inner",
             in_nested);
    if (in_unnamed != "Top.outer")
      $fatal(1, "%%m in the unnamed block was %s, expected Top.outer",
             in_unnamed);
    if (in_labeled != "Top.outer.tagged_stmt")
      $fatal(1, "%%m in the labeled statement was %s, expected %s",
             in_labeled, "Top.outer.tagged_stmt");
    if (in_function != "Top.stamp")
      $fatal(1, "%%m in the function was %s, expected Top.stamp", in_function);
    if (g_chosen.in_generate != "Top.g_chosen")
      $fatal(1, "%%m in the generate block was %s, expected Top.g_chosen",
             g_chosen.in_generate);
    if (g_repeated[0].in_generate != "Top.g_repeated[0]")
      $fatal(1, "%%m in generate instance 0 was %s, expected %s",
             g_repeated[0].in_generate, "Top.g_repeated[0]");
    if (g_repeated[1].in_generate != "Top.g_repeated[1]")
      $fatal(1, "%%m in generate instance 1 was %s, expected %s",
             g_repeated[1].in_generate, "Top.g_repeated[1]");
    $display("All checks passed");
  end
endmodule
