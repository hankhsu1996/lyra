// A module's name is in the definitions name space and a class a module
// declares is in the module's own (LRM 3.13), so a module may declare a class
// under its own name, and a generate block a class under the block's label.
module Top;
  class Top;
    int v = 7;
    function int Twice();
      return v * 2;
    endfunction
  endclass

  Top held = new;

  // A class is a scope too (LRM 8.23), so a class declared inside another may
  // carry the name of one declared beside the outer.
  class Plain;
    int a = 1;
  endclass
  class Outer;
    class Plain;
      int b = 2;
      int c = 3;
    endclass
    Plain nested = new;
  endclass
  Plain beside = new;
  Outer outer = new;

  // The two are different types (LRM 8.3), so a generic class bound to each is
  // two specializations (LRM 8.25).
  class Box #(type T = int);
    T held = new;
  endclass
  Box #(Plain) box_of_beside = new;
  Box #(Outer::Plain) box_of_nested = new;

  // A class whose escaped name spells a generic class's name and more is a
  // class of its own beside every specialization of that one.
  class \Box__854ba412c604ec55 ;
    int beside_the_boxes = 51;
    int and_more = 52;
  endclass
  \Box__854ba412c604ec55 spelled_alike = new;

  // An escaped name may hold the characters a class scope is written with
  // (LRM 5.6.1), and each class keeps its own static property (LRM 8.9).
  class \Keeper::Kept ;
    static int count = 4;
  endclass
  class Keeper;
    class Kept;
      static int count = 5;
    endclass
  endclass

  // A structure is the type its declaration makes it (LRM 6.22.1), and a class
  // is a scope a type may be declared in, so a structure declared in a class
  // whose escaped name spells a path and one declared in the class at that
  // path are two types.
  class \a.a_b ;
    typedef struct {int first;} S;
    S s = '{first: 11};
  endclass
  class a;
    class b;
      typedef struct {
        int first;
        int second;
      } S;
      S s = '{first: 12, second: 13};
    endclass
  endclass
  \a.a_b escaped = new;
  a::b pathed = new;

  // A subroutine and a block inside one are scopes too (LRM 23.9), so two
  // functions may each declare a structure under one name, and a block with no
  // label may declare one. A structure written in place of a name inside
  // another answers to no name of its own (LRM 6.22.1).
  function automatic int FromFirst();
    typedef struct {int x;} Local;
    Local held = '{x: 41};
    return held.x;
  endfunction
  function automatic int FromSecond();
    typedef struct {
      int x;
      int y;
    } Local;
    Local held = '{x: 42, y: 43};
    return held.x + held.y;
  endfunction
  function automatic int FromBlock();
    int total = 0;
    begin
      typedef struct {int z;} Inner;
      Inner held = '{z: 44};
      total = held.z;
    end
    return total;
  endfunction
  struct {
    struct {int a;} inner;
    int b;
  } wrapping;

  if (1) begin : g
    class g;
      int w = 9;
    endclass
    g inner = new;
  end

  initial begin
    if (held.v !== 7) $fatal(1, "the class holds %0d", held.v);
    if (held.Twice() !== 14) $fatal(1, "the class answers %0d", held.Twice());
    if (beside.a !== 1) $fatal(1, "the class beside holds %0d", beside.a);
    if (outer.nested.c !== 3)
      $fatal(1, "the nested class holds %0d", outer.nested.c);
    if (g.inner.w !== 9) $fatal(1, "the block's class holds %0d", g.inner.w);
    if (box_of_beside.held.a !== 1)
      $fatal(1, "the box of the class beside holds %0d", box_of_beside.held.a);
    if (box_of_nested.held.c !== 3)
      $fatal(1, "the box of the nested class holds %0d", box_of_nested.held.c);
    if (spelled_alike.beside_the_boxes !== 51)
      $fatal(1, "the class spelled like a box holds %0d",
             spelled_alike.beside_the_boxes);
    if (spelled_alike.and_more !== 52)
      $fatal(1, "the class spelled like a box holds %0d second",
             spelled_alike.and_more);
    if (\Keeper::Kept ::count !== 4)
      $fatal(1, "the escaped class keeps %0d", \Keeper::Kept ::count);
    if (Keeper::Kept::count !== 5)
      $fatal(1, "the nested class keeps %0d", Keeper::Kept::count);
    if (FromFirst() !== 41)
      $fatal(1, "the first function's structure holds %0d", FromFirst());
    if (FromSecond() !== 85)
      $fatal(1, "the second function's structure sums to %0d", FromSecond());
    if (FromBlock() !== 44)
      $fatal(1, "the block's structure holds %0d", FromBlock());
    wrapping.inner.a = 45;
    wrapping.b = 46;
    if (wrapping.inner.a !== 45)
      $fatal(1, "the nested structure holds %0d", wrapping.inner.a);
    if (wrapping.b !== 46)
      $fatal(1, "the wrapping structure holds %0d", wrapping.b);
    if (escaped.s.first !== 11)
      $fatal(1, "the escaped class's structure holds %0d", escaped.s.first);
    if (pathed.s.first !== 12)
      $fatal(1, "the nested class's structure holds %0d", pathed.s.first);
    if (pathed.s.second !== 13)
      $fatal(1, "the nested class's structure holds %0d second",
             pathed.s.second);
    $display("All checks passed");
  end
endmodule
