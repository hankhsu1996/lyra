// A class declared inside a module or a generate block is a type of that
// instance, so an object of it belongs to the one instance it was created in
// and its body reaches that instance's own declarations -- reading a variable,
// writing one, and calling a function the scope declares (LRM 6.22, 23.9).
// Two instances of one module therefore carry two unrelated sets of objects:
// each reaches its own instance's storage and never the other's, which is what
// the differing seeds below distinguish.
module Holder (input int seed);
  int slot = -1;
  int read_back = -1;
  int doubled = -1;

  function automatic int twice(int v);
    return v * 2;
  endfunction

  class Peeker;
    function int peek();
      return slot;
    endfunction

    function void poke(int v);
      slot = v;
    endfunction

    function int through_scope();
      return twice(slot);
    endfunction
  endclass

  initial begin
    Peeker p = new();
    p.poke(seed);
    read_back = p.peek();
    doubled = p.through_scope();
  end
endmodule

module Top;
  Holder u1 (.seed(10));
  Holder u2 (.seed(21));

  if (1) begin : g
    int owned = 3;

    class Toucher;
      function void bump();
        owned = owned + 4;
      endfunction
    endclass

    initial begin
      Toucher t = new();
      t.bump();
    end
  end

  final begin
    if (u1.slot !== 10) $fatal(1, "u1.slot was %0d, expected 10", u1.slot);
    if (u2.slot !== 21) $fatal(1, "u2.slot was %0d, expected 21", u2.slot);
    if (u1.read_back !== 10)
      $fatal(1, "u1.read_back was %0d, expected 10", u1.read_back);
    if (u2.read_back !== 21)
      $fatal(1, "u2.read_back was %0d, expected 21", u2.read_back);
    if (u1.doubled !== 20)
      $fatal(1, "u1.doubled was %0d, expected 20", u1.doubled);
    if (u2.doubled !== 42)
      $fatal(1, "u2.doubled was %0d, expected 42", u2.doubled);
    if (g.owned !== 7) $fatal(1, "g.owned was %0d, expected 7", g.owned);
    $display("All checks passed");
  end
endmodule
