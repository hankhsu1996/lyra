// Hierarchical instantiation creates an instance of a module, and the same
// module may be instantiated more than once (LRM 23.3.2). Each instance is a
// separate branch of the name hierarchy holding its own copy of the module's
// variables, and each runs its own copy of the module's procedures (LRM 23.6).
// An instance that instantiates in turn adds a further level, so the tree is
// as deep as the chain of instantiations.
module Counter;
  int ticks;
  int tag;

  initial begin
    ticks = 0;
    #1 ticks = ticks + 1;
    #1 ticks = ticks + 1;
  end
endmodule

module Mid;
  int mid_value;
  Counter deep();

  initial mid_value = 9;
endmodule

module Top;
  Counter first();
  Counter second();
  Mid nested();

  initial begin
    #1;
    first.tag = 4;
    second.tag = 6;
  end

  final begin
    if (first.ticks !== 2)
      $fatal(1, "first.ticks was %0d, expected 2", first.ticks);
    if (second.ticks !== 2)
      $fatal(1, "second.ticks was %0d, expected 2", second.ticks);
    if (nested.deep.ticks !== 2)
      $fatal(1, "nested.deep.ticks was %0d, expected 2", nested.deep.ticks);
    if (first.tag !== 4) $fatal(1, "first.tag was %0d, expected 4", first.tag);
    if (second.tag !== 6)
      $fatal(1, "second.tag was %0d, expected 6", second.tag);
    if (nested.deep.tag !== 0)
      $fatal(1, "nested.deep.tag was %0d, expected 0", nested.deep.tag);
    if (nested.mid_value !== 9)
      $fatal(1, "nested.mid_value was %0d, expected 9", nested.mid_value);
    $display("All checks passed");
  end
endmodule
