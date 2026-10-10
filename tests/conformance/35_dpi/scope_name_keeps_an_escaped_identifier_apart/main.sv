// The context of an imported subroutine is the fully qualified name of the
// subroutine minus the subroutine name itself, a scope answers for that name,
// and a name finds its scope again (LRM Annex H.9.2, H.9.3). A fully qualified
// name separates the names it is made of by periods, and an escaped
// identifier among them is followed by white space and then the period (LRM
// 23.6), so a block whose escaped label spells two names and a period is
// named apart from a block of the second name inside a block of the first:
// each import reports its own scope's name, and each name finds the scope it
// came from and not the other.
module Top;
  string nested_name = "unset";
  string escaped_name = "unset";
  int nested_finds = 0;
  int escaped_finds = 0;

  if (1) begin : a
    if (1) begin : b
      import "DPI-C" context function string scope_name();
      import "DPI-C" context function int name_finds_this_scope(
          input string name, input string other);
      initial begin
        nested_name = scope_name();
        nested_finds = name_finds_this_scope("Top.a.b", "Top.\\a.b ");
      end
    end
  end

  if (1) begin : \a.b
    import "DPI-C" context function string scope_name();
    import "DPI-C" context function int name_finds_this_scope(
        input string name, input string other);
    initial begin
      escaped_name = scope_name();
      escaped_finds = name_finds_this_scope("Top.\\a.b ", "Top.a.b");
    end
  end

  final begin
    if (nested_name != "Top.a.b")
      $fatal(1, "the block in a block was named '%s'", nested_name);
    if (escaped_name != "Top.\\a.b ")
      $fatal(1, "the escaped block was named '%s'", escaped_name);
    if (nested_finds !== 3)
      $fatal(1, "the nested name's checks reported %0d, expected 3",
             nested_finds);
    if (escaped_finds !== 3)
      $fatal(1, "the escaped name's checks reported %0d, expected 3",
             escaped_finds);
    $display("All checks passed");
  end
endmodule
