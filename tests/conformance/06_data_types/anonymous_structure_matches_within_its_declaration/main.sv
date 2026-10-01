// A structure type written in place, with no typedef naming it, matches itself
// among the data objects declared in the same declaration statement and no
// other type (LRM 6.22.1 c), so two objects declared together may be assigned
// and compared as one type. Such a type may be written wherever a data object
// is declared -- in a module, in an unnamed block, in a block nested in
// another, in a function, as a class property, or as a member of another
// structure -- and a typedef inside a generate loop declares a type of its own
// for each iteration (LRM 6.22). Each is a structure type with every
// whole-value operation (LRM 7.2, 11.4.5, 20.9).
module Top;
  struct {
    int count;
    logic [3:0] tag;
  } first, second;

  struct {
    int count;
    struct {
      string label;
      logic flag;
    } detail;
  } outer_value, outer_copy;

  class Holder;
    struct {
      int count;
      logic [3:0] tag;
    } held;
  endclass

  for (genvar i = 0; i < 2; i++) begin : g
    typedef struct {
      int index;
      logic flag;
    } per_iteration_t;
    per_iteration_t entry = '{i, 1'bx};
  end

  function automatic int plus_one(int x);
    struct { int value; } scratch;
    scratch.value = x;
    return scratch.value + 1;
  endfunction

  logic declared_together_equal;
  logic declared_together_unknown;
  logic nested_equal;
  string nested_label;
  int outer_block_count;
  string inner_block_label;
  logic held_unknown;
  int function_result;

  initial begin
    Holder holder;

    first = '{5, 4'b0011};
    second = first;
    declared_together_equal = second == first;
    second.tag = 4'bx011;
    declared_together_unknown = $isunknown(second);

    outer_value.count = 1;
    outer_value.detail.label = "inner";
    outer_value.detail.flag = 1'b0;
    outer_copy = outer_value;
    nested_equal = outer_copy == outer_value;
    nested_label = outer_copy.detail.label;

    begin
      struct {
        int count;
        string label;
      } in_block;
      in_block.count = 3;
      in_block.label = "outer";
      begin
        struct {
          int count;
          string label;
        } in_nested_block;
        in_nested_block.count = in_block.count;
        in_nested_block.label = "inner";
        outer_block_count = in_nested_block.count;
        inner_block_label = in_nested_block.label;
      end
    end

    holder = new;
    held_unknown = $isunknown(holder.held);
    function_result = plus_one(41);
  end

  final begin
    if (declared_together_equal !== 1'b1)
      $fatal(1, "objects declared together did not compare equal after a copy");
    if (declared_together_unknown !== 1'b1)
      $fatal(1, "an x member was not seen by $isunknown");
    if (nested_equal !== 1'b1)
      $fatal(1, "a structure holding one written in place differed from its copy");
    if (nested_label != "inner")
      $fatal(1, "nested_label was %s, expected inner", nested_label);
    if (outer_block_count !== 3)
      $fatal(1, "outer_block_count was %0d, expected 3", outer_block_count);
    if (inner_block_label != "inner")
      $fatal(1, "inner_block_label was %s, expected inner", inner_block_label);
    if (held_unknown !== 1'b1)
      $fatal(1, "a class property of structure type did not start unknown");
    if (function_result !== 42)
      $fatal(1, "function_result was %0d, expected 42", function_result);
    if (g[0].entry.index !== 0 || g[1].entry.index !== 1)
      $fatal(1, "each generate iteration did not keep its own entry");
    if ($isunknown(g[1].entry) !== 1'b1)
      $fatal(1, "a generate iteration's structure did not start with an x flag");
    $display("All checks passed");
  end
endmodule
