// A property is read through any expression naming an object (LRM 8.4),
// including the handle a function returns, and an always_comb is sensitive to
// what any function it calls reads (LRM 9.2.2.2.1). So a block calling a
// function that reads a property through the handle another function returns
// holds that property's value, and holds the new object's once the variable
// the inner function reads is given another.
module Top;
  class Named;
    string tag;
    function new(string tag);
      this.tag = tag;
    endfunction
  endclass

  Named current = new("the first object's tag, past any short string");
  string seen;

  function automatic Named pick();
    return current;
  endfunction

  function automatic string tag_of_picked();
    return pick().tag;
  endfunction

  always_comb seen = tag_of_picked();

  string first_seen;

  initial begin
    #1;
    first_seen = seen;
    current = new("the second object's tag, past any short string");
    #1;
  end

  final begin
    if (first_seen != "the first object's tag, past any short string")
      $fatal(1, "the block first held '%s'", first_seen);
    if (seen != "the second object's tag, past any short string")
      $fatal(1, "the block then held '%s'", seen);
    $display("All checks passed");
  end
endmodule
