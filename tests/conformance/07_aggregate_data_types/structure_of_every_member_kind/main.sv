// An unpacked structure's members may be of any data type (LRM 7.2), and the
// structure is assigned, compared and watched as a whole: an assignment copies
// every member (LRM 7.2.2), `==` holds when every pair of members is equal and
// is unknown where none is unequal and one is unknown (LRM 11.4.5), and a write
// that changes any member is a change of the variable (LRM 4.3). Every member
// kind a structure can hold is here at once, so each of those whole-value
// questions is asked of each kind.
module Top;
  class Cell;
    int v;

    function new(int v);
      this.v = v;
    endfunction
  endclass

  typedef union {
    int  as_int;
    byte as_byte;
  } Overlay;

  typedef union tagged {
    void none;
    int  some;
  } Maybe;

  typedef struct {
    logic [3:0] bits;
    int         count;
  } Inner;

  typedef struct {
    logic [7:0] vector;
    string      text;
    real        ratio;
    chandle     opaque;
    Cell        handle;
    Overlay     overlay;
    Maybe       maybe;
    int         fixed [2];
    int         grown [];
    int         lined [$];
    int         keyed [string];
    Inner       inner;
  } Everything;

  Everything original;
  Everything copy;
  Everything mirror;

  logic equal_after_copy;
  logic equal_after_change;
  logic equal_with_unknown;
  int   mirrored;

  always_comb mirror = original;

  initial begin
    // Every target starts at a value its own check rejects, so a check that
    // never ran cannot pass as one that answered correctly.
    equal_after_copy = 1'b0;
    equal_after_change = 1'b1;
    equal_with_unknown = 1'b0;
    mirrored = 0;

    original.vector = 8'h5A;
    original.text = "text";
    original.ratio = 0.5;
    original.opaque = null;
    original.handle = new(7);
    original.overlay.as_int = 3;
    original.maybe = tagged some 4;
    original.fixed = '{1, 2};
    original.grown = new[2];
    original.grown[1] = 9;
    original.lined.push_back(5);
    original.keyed["k"] = 6;
    original.inner.bits = 4'b1010;
    original.inner.count = 8;

    copy = original;
    equal_after_copy = (copy == original);

    copy.lined.push_back(6);
    equal_after_change = (copy == original);

    copy = original;
    copy.inner.bits = 4'b10x0;
    equal_with_unknown = (copy == original);

    #1;
    mirrored = (mirror.lined.size() == 1 && mirror.keyed["k"] == 6 &&
                 mirror.handle.v == 7 && mirror.inner.count == 8);
    original.maybe = tagged none;
    #1;
    if (mirror.maybe matches tagged some .v) mirrored = 0;

    if (equal_after_copy !== 1'b1)
      $fatal(1, "a copy compared as %b to what it copied", equal_after_copy);
    if (equal_after_change !== 1'b0)
      $fatal(1, "a changed member compared as %b", equal_after_change);
    if (equal_with_unknown !== 1'bx)
      $fatal(1, "an unknown member compared as %b", equal_with_unknown);
    if (mirrored !== 1) $fatal(1, "a change of the structure was not seen");
    $display("All checks passed");
  end
endmodule
