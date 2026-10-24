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
// instance was elaborated (LRM 21.2.1.5, 23.6, 9.3.5, 27.5, 27.4). What a
// module's parameters are set to is no part of any of that, so a parameterized
// module contributes the name it was declared or instantiated under like any
// other, at the top level as well as below it.
//
// A package is a design element and a class method is a subroutine, so %m
// written in either names it too (LRM 3.2, 21.2.1.5). Neither lies in the
// instance hierarchy, so the standard does not spell how a package or a class
// joins the name, and what is checked of those is only what it fixes: the name
// ends with the subroutine and the named block written there, it carries the
// package or class around them, and a class declared in a module names the
// instance it belongs to first. A static variable's initializer runs once,
// apart from every call, but it is written in the scope declaring the
// variable, so that is the scope it names (LRM 6.21). A specialization of a
// class on a type a generate block declares is a type of that block (LRM 8.25,
// 6.22), and still a class the module declares, so its methods name the
// module's instance and not the block.
package tools;
  function automatic string where();
    return $sformatf("%m");
  endfunction

  function automatic string where_in_block();
    string text;
    begin : nested
      text = $sformatf("%m");
    end
    return text;
  endfunction

  function automatic string where_kept();
    static string kept = $sformatf("%m");
    return kept;
  endfunction

  class Tool;
    static string at_static = $sformatf("%m");
    string at_construction;

    function new();
      at_construction = $sformatf("%m");
    endfunction

    function string method_name();
      return $sformatf("%m");
    endfunction

    static function string static_name();
      return $sformatf("%m");
    endfunction
  endclass
endpackage

// Whether `text` begins with `head` and whether it ends with `tail`.
function automatic bit starts_with(string text, string head);
  return text.len() >= head.len() && text.substr(0, head.len() - 1) == head;
endfunction

function automatic bit ends_with(string text, string tail);
  return text.len() >= tail.len() &&
         text.substr(text.len() - tail.len(), text.len() - 1) == tail;
endfunction

// Whether `part` appears anywhere in `text`.
function automatic bit holds(string text, string part);
  for (int i = 0; i + part.len() <= text.len(); i++) begin
    if (text.substr(i, i + part.len() - 1) == part) return 1;
  end
  return 0;
endfunction

module Leaf;
  parameter int Depth = 3;

  string in_instance;
  string in_task;
  string in_local_class;
  string in_kept_block;
  string in_kept_function;
  string in_deep_specialization;

  class Local;
    function string method_name();
      return $sformatf("%m");
    endfunction
  endclass

  class Holder #(type T = int);
    function string method_name();
      return $sformatf("%m");
    endfunction
  endclass

  if (1) begin : g_deep
    typedef struct packed { bit [3:0] f; } inner_t;
    initial begin
      Holder #(inner_t) held;
      held = new;
      in_deep_specialization = held.method_name();
    end
  end

  task automatic stamp();
    in_task = $sformatf("%m");
  endtask

  function automatic string kept_in_function();
    static string kept = $sformatf("%m");
    return kept;
  endfunction

  initial begin : kept
    static string kept_here = $sformatf("%m");
    in_kept_block = kept_here;
    in_kept_function = kept_in_function();
  end

  initial begin
    Local local_object;
    in_instance = $sformatf("%m");
    stamp();
    local_object = new;
    in_local_class = local_object.method_name();
  end
endmodule

module Top;
  parameter int Width = 8;

  string at_top;
  string in_named;
  string in_nested;
  string in_unnamed;
  string in_labeled;
  string in_function;
  string in_package;
  string in_package_block;
  string in_package_kept;
  string in_package_class;
  string in_package_constructor;
  string in_static_method;
  string in_static_property;

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

  initial begin
    tools::Tool tool;
    tool = new;
    in_package = tools::where();
    in_package_block = tools::where_in_block();
    in_package_class = tool.method_name();
    in_package_constructor = tool.at_construction;
    in_static_method = tools::Tool::static_name();
    in_package_kept = tools::where_kept();
    in_static_property = tools::Tool::at_static;
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
    if (!starts_with(leaf.in_deep_specialization, "Top.leaf.") ||
        starts_with(leaf.in_deep_specialization, "Top.leaf.g_deep") ||
        !holds(leaf.in_deep_specialization, "Holder") ||
        !ends_with(leaf.in_deep_specialization, "method_name"))
      $fatal(1, "%%m in a class specialized on a block's type was %s",
             leaf.in_deep_specialization);
    if (leaf.in_kept_block != "Top.leaf.kept")
      $fatal(1, "%%m in a static's initializer in a block was %s, expected %s",
             leaf.in_kept_block, "Top.leaf.kept");
    if (leaf.in_kept_function != "Top.leaf.kept_in_function")
      $fatal(1, "%%m in a function static's initializer was %s, expected %s",
             leaf.in_kept_function, "Top.leaf.kept_in_function");
    if (!starts_with(leaf.in_local_class, "Top.leaf.") ||
        !holds(leaf.in_local_class, "Local") ||
        !ends_with(leaf.in_local_class, "method_name"))
      $fatal(1, "%%m in a class of the instance was %s", leaf.in_local_class);
    if (!starts_with(in_package, "tools") || !ends_with(in_package, "where"))
      $fatal(1, "%%m in a package function was %s", in_package);
    if (!starts_with(in_package_block, "tools") ||
        !ends_with(in_package_block, "where_in_block.nested"))
      $fatal(1, "%%m in a package function's block was %s", in_package_block);
    if (!starts_with(in_package_class, "tools") ||
        !holds(in_package_class, "Tool") ||
        !ends_with(in_package_class, "method_name"))
      $fatal(1, "%%m in a package class's method was %s", in_package_class);
    if (!holds(in_package_constructor, "Tool") ||
        !ends_with(in_package_constructor, "new"))
      $fatal(1, "%%m in a package class's constructor was %s",
             in_package_constructor);
    if (!starts_with(in_package_kept, "tools") ||
        !ends_with(in_package_kept, "where_kept"))
      $fatal(1, "%%m in a package function static's initializer was %s",
             in_package_kept);
    if (!starts_with(in_static_property, "tools") ||
        !ends_with(in_static_property, "Tool"))
      $fatal(1, "%%m in a static property's initializer was %s",
             in_static_property);
    if (!holds(in_static_method, "Tool") ||
        !ends_with(in_static_method, "static_name"))
      $fatal(1, "%%m in a static method was %s", in_static_method);
    $display("All checks passed");
  end
endmodule
