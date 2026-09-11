// A hierarchical name reaches a declaration of the named scope whatever its
// type is (LRM 23.8), so it reaches a variable holding a handle to an object of
// a class that scope declares (LRM 8.3). The name reaches the variable, never
// the class: what a reader may do with the handle it read is hold it, compare
// it against another handle of the same type, and compare it against null
// (LRM 8.4). That holds in both directions of the hierarchy, since a downward
// name and an upward one differ only in how the head is found.
module Child;
  class Token;
    int tag;
    function new(int t);
      tag = t;
    endfunction
  endclass

  Token mine;
  Token spare;
  bit parent_handle_is_set;
  bit parent_handles_are_distinct;
  bit parent_handle_matches_itself;

  initial begin
    mine = new(5);
    spare = mine;
  end

  initial begin
    #1;
    parent_handle_is_set = (Top.owned != null);
    parent_handles_are_distinct = (Top.owned != Top.other);
    parent_handle_matches_itself = (Top.owned == Top.owned);
  end
endmodule

module Top;
  class Record;
    int id;
    function new(int i);
      id = i;
    endfunction
  endclass

  Record owned;
  Record other;

  Child kid ();

  initial begin
    owned = new(1);
    other = new(2);
  end

  final begin
    if (kid.mine == null) $fatal(1, "a downward name read a null handle");
    if (kid.mine != kid.spare)
      $fatal(1, "two downward names of one object compared unequal");
    if (!kid.parent_handle_is_set)
      $fatal(1, "an upward name read a null handle");
    if (!kid.parent_handles_are_distinct)
      $fatal(1, "upward names of two objects compared equal");
    if (!kid.parent_handle_matches_itself)
      $fatal(1, "two upward names of one object compared unequal");
    $display("All checks passed");
  end
endmodule
