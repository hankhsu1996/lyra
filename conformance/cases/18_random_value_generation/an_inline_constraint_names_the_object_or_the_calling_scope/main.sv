// In the constraints a `randomize() with` call writes, a name is looked up in
// the class of the object the call randomizes first, and then where the call is
// written; a dotted name starts the same way, `this` and `super` name the
// object and its class's base, and a restricted block sends only the names it
// lists into the object (LRM 18.7). `local::` starts where the call is written,
// `local::this` and `local::super` included (LRM 18.7.1). `Caller` and
// `Target` below share a base, so every name is found on both sides and only
// the rule decides which object it reaches.
module Top;
  int k = 99;
  int checks = 0;
  int failures = 0;

  function automatic void check(int got, int want, string what);
    checks++;
    if (got !== want) begin
      failures++;
      $display("%s was %0d, expected %0d", what, got, want);
    end
  endfunction

  class Leaf;
    int x;
  endclass

  class Base;
    int k;
    static int shared = 5;
    function int get_k();
      return k;
    endfunction
    virtual function int kind();
      return 10 + k;
    endfunction
    static function int six();
      return 6;
    endfunction
  endclass

  class Middle extends Base;
    virtual function int kind();
      return 15 + k;
    endfunction
  endclass

  class Target extends Base;
    rand int v;
    rand int w;
    Leaf leaf;
    function new;
      leaf = new;
    endfunction
    virtual function int kind();
      return 20;
    endfunction

    // The object randomizes itself, so the object and the caller are one.
    function void on_itself();
      k = 2;
      v = -1;
      w = -1;
      check(randomize() with { v == k; w == this.k + 1; }, 1, "own call");
      check(v, 2, "own call: k");
      check(w, 3, "own call: this.k + 1");
      v = -1;
      w = -1;
      check(this.randomize() with { v == local::k; w == k; }, 1,
            "own call through this");
      check(v, 2, "own call through this: local::k");
      check(w, 2, "own call through this: k");
    endfunction

    // The caller is of the object's own class.
    function void on_another(Target other);
      k = 8;
      other.k = 9;
      other.v = -1;
      other.w = -1;
      check(other.randomize() with { v == k; w == local::k; }, 1,
            "same class");
      check(other.v, 9, "same class: k");
      check(other.w, 8, "same class: local::k");
    endfunction
  endclass

  class Caller extends Middle;
    Leaf leaf;
    Target arr[2];
    function new;
      leaf = new;
    endfunction
    virtual function int kind();
      return 30;
    endfunction

    function void run(Target t);
      k = 7;
      leaf.x = 70;
      t.k = 1;
      t.leaf.x = 10;

      t.v = -1;
      t.w = -1;
      check(t.randomize() with { v == k; w == get_k(); }, 1, "bare");
      check(t.v, 1, "bare k");
      check(t.w, 1, "bare get_k()");

      t.v = -1;
      t.w = -1;
      check(t.randomize() with { v == local::k; w == local::get_k(); }, 1,
            "local::");
      check(t.v, 7, "local::k");
      check(t.w, 7, "local::get_k()");

      t.v = -1;
      t.w = -1;
      check(t.randomize() with { v == this.k; w == this.get_k(); }, 1,
            "this.");
      check(t.v, 1, "this.k");
      check(t.w, 1, "this.get_k()");

      t.v = -1;
      t.w = -1;
      check(t.randomize() with {
        v == local::this.k;
        w == local::this.get_k();
      }, 1, "local::this.");
      check(t.v, 7, "local::this.k");
      check(t.w, 7, "local::this.get_k()");

      t.v = -1;
      t.w = -1;
      check(t.randomize() with { v == super.kind(); w == super.k; }, 1,
            "super.");
      check(t.v, 11, "super.kind()");
      check(t.w, 1, "super.k");

      t.v = -1;
      t.w = -1;
      check(t.randomize() with {
        v == local::super.kind();
        w == local::super.k;
      }, 1, "local::super.");
      check(t.v, 22, "local::super.kind()");
      check(t.w, 7, "local::super.k");

      t.v = -1;
      check(t.randomize() with { v == this.super.kind(); }, 1, "this.super.");
      check(t.v, 11, "this.super.kind()");

      t.v = -1;
      t.w = -1;
      check(t.randomize() with { v == kind(); w == local::kind(); }, 1,
            "a virtual method");
      check(t.v, 20, "kind()");
      check(t.w, 30, "local::kind()");

      t.v = -1;
      t.w = -1;
      check(t.randomize() with (v, w) { v == k; w == get_k(); }, 1,
            "restricted, not listed");
      check(t.v, 7, "restricted k");
      check(t.w, 7, "restricted get_k()");

      t.v = -1;
      check(t.randomize() with (v, k) { v == k; }, 1, "restricted, listed");
      check(t.v, 1, "restricted, listed k");

      t.v = -1;
      t.w = -1;
      check(t.randomize() with { v == leaf.x; w == local::leaf.x; }, 1,
            "dotted");
      check(t.v, 10, "leaf.x");
      check(t.w, 70, "local::leaf.x");

      t.v = -1;
      t.w = -1;
      check(t.randomize() with { v == shared; w == six(); }, 1, "static");
      check(t.v, 5, "a static property");
      check(t.w, 6, "a static method");

      arr[0] = new;
      arr[1] = new;
      arr[0].k = 3;
      arr[1].k = 4;
      arr[1].v = -1;
      arr[1].w = -1;
      check(arr[1].randomize() with { v == this.k; w == k; }, 1,
            "an element's object");
      check(arr[1].v, 4, "an element's this.k");
      check(arr[1].w, 4, "an element's k");
      check(arr[0].v, 0, "the element not randomized");
    endfunction
  endclass

  // A caller sharing no class with the object.
  class Stranger;
    int k;
    function void run(Target t);
      k = 40;
      t.k = 5;
      t.v = -1;
      t.w = -1;
      check(t.randomize() with { v == k; w == local::k; }, 1, "unrelated");
      check(t.v, 5, "unrelated: k");
      check(t.w, 40, "unrelated: local::k");
    endfunction
  endclass

  initial begin
    Caller c;
    Stranger s;
    Target t;
    Target other;
    c = new;
    s = new;
    t = new;
    other = new;
    c.run(t);
    t.on_itself();
    t.on_another(other);
    s.run(t);

    // A module's procedure has no object of its own, so `local::` reaches the
    // module's name.
    t.k = 11;
    t.v = -1;
    t.w = -1;
    check(t.randomize() with { v == k; w == local::k; }, 1, "a module's call");
    check(t.v, 11, "a module's call: k");
    check(t.w, 99, "a module's call: local::k");
  end

  final begin
    if (checks !== 53) $fatal(1, "%0d checks ran, expected 53", checks);
    if (failures !== 0) $fatal(1, "%0d checks failed", failures);
    $display("All checks passed");
  end
endmodule
