// A class may extend a class declared in a scope enclosing its own, and each
// class in the lineage keeps the scope it was written in: a name in a method
// is searched outward from where that method's class is declared (LRM 23.9),
// and each instance with a type declared inside it has a type of its own
// (LRM 6.22). So an inherited method reads the declaring instance of the class
// that wrote it, which encloses the instance the extending class belongs to,
// however many generate levels lie between the two. Arguments handed to the
// base constructor are evaluated before the extending class's own
// construction begins (LRM 8.7), and still reach the scope that declares it.
package lib;
  class Named;
    int tag;
    function new(int t);
      tag = t;
    endfunction
  endclass
endpackage

module Holder (input int seed);
  int sum[3];
  int lineage = -1;
  int passed = -1;

  virtual class Base;
    int v;
    function new(int x);
      v = x;
    endfunction
    function int outer();
      return seed;
    endfunction
    static function int doubled_seed();
      return seed * 2;
    endfunction
    virtual function int described();
      return seed + 1000;
    endfunction
    pure virtual function int get();
  endclass

  class Mid extends Base;
    function new(int x);
      super.new(x + seed);
    endfunction
    function int get();
      return v;
    endfunction
  endclass

  class Direct extends Base;
    function new();
      super.new(seed * 2);
    endfunction
    function int get();
      return v;
    endfunction
  endclass

  for (genvar i = 0; i < 3; i++) begin : g
    int own = 100 * (i + 1);

    class Leaf extends Base;
      function new();
        super.new(own);
      endfunction
      function int get();
        return own + outer() + v;
      endfunction
      function int described();
        return own + super.described();
      endfunction
    endclass

    class Chained extends Mid;
      function new();
        super.new(own);
      endfunction
    endclass

    class Tagged extends lib::Named;
      function new();
        super.new(own + 1);
      endfunction
    endclass

    if (1) begin : inner
      int deep = i;

      class Deeper extends Base;
        function new();
          super.new(deep);
        endfunction
        function int get();
          return deep + outer() + v;
        endfunction
      endclass

      class Sibling extends Deeper;
        function int get();
          return deep + v;
        endfunction
      endclass

      initial begin
        Deeper d;
        Sibling s;
        #1;
        d = new();
        s = new();
        sum[i] = d.get() + s.get();
      end
    end

    initial begin
      Leaf l;
      Chained c;
      Tagged t;
      #2;
      l = new();
      c = new();
      t = new();
      if (l.get() !== 2 * own + seed)
        $fatal(1, "g[%0d] Leaf.get() was %0d", i, l.get());
      if (l.described() !== own + seed + 1000)
        $fatal(1, "g[%0d] Leaf.described() was %0d", i, l.described());
      if (Leaf::doubled_seed() !== 2 * seed)
        $fatal(1, "g[%0d] Leaf::doubled_seed() was %0d", i, Leaf::doubled_seed());
      if (c.get() !== own + seed)
        $fatal(1, "g[%0d] Chained.get() was %0d", i, c.get());
      if (t.tag !== own + 1) $fatal(1, "g[%0d] Tagged.tag was %0d", i, t.tag);
    end
  end

  initial begin
    Direct d;
    #3;
    d = new();
    passed = d.get();
    lineage = sum[0] + sum[1] + sum[2];
  end
endmodule

module Top;
  Holder u1 (.seed(10));
  Holder u2 (.seed(21));

  final begin
    // Each Deeper contributes deep + seed + deep and each Sibling deep + deep,
    // with deep running 0, 1 and 2.
    if (u1.lineage !== 12 + 3 * 10)
      $fatal(1, "u1.lineage was %0d, expected %0d", u1.lineage, 12 + 3 * 10);
    if (u2.lineage !== 12 + 3 * 21)
      $fatal(1, "u2.lineage was %0d, expected %0d", u2.lineage, 12 + 3 * 21);
    if (u1.passed !== 20) $fatal(1, "u1.passed was %0d, expected 20", u1.passed);
    if (u2.passed !== 42) $fatal(1, "u2.passed was %0d, expected 42", u2.passed);
    $display("All checks passed");
  end
endmodule
