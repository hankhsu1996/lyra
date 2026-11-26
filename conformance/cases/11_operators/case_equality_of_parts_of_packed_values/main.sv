// The equality operators compare their operands bit for bit, and case
// equality counts x and z bits as bits that have to match (LRM 11.4.5), so
// an operand that is a part of a packed value -- a member of a packed
// structure or union, a member of one held inside another, an element of a
// packed array at a constant or a computed index, a member of a structure a
// class property holds -- compares as the value it holds, on either side of
// the operator, against a whole value or against another part. That holds
// at every width, in a module's initial procedure, in a function given the
// values as arguments and in a class method, and in a case item's
// expression (LRM 12.5). Each part holds its value beside other bits of what
// it is part of, so none of them is the whole.
module Parts #(
    int W = 8
);
  typedef struct packed {
    logic [W-1:0] c;
    logic tail;
  } held_t;
  typedef struct packed {
    logic head;
    held_t inner;
  } nest_t;
  typedef union packed {
    logic [W:0] bits;
    held_t as_held;
  } either_t;

  class Box;
    held_t h;
    function bit Same(logic [W-1:0] v);
      return h.c === v;
    endfunction
    function bit Differs(logic [W-1:0] v);
      return h.c !== v;
    endfunction
  endclass

  function automatic bit SameMembers(held_t a, held_t b);
    return a.c === b.c;
  endfunction

  logic [W-1:0] value, low_differs, high_differs, with_x, with_z;
  held_t s, t;
  nest_t n;
  either_t u;
  logic [3:0][W-1:0] row;
  Box box;
  int i;
  int checks;
  bit done;

  task automatic Expect(string what, bit got, bit want);
    if (got !== want)
      $fatal(1, "W=%0d: %s answered %b, expected %b", W, what, got, want);
    checks++;
  endtask

  initial begin
    done = 0;
    checks = 0;
    value = '0;
    value[0] = 1;
    value[W/2] = 1;
    value[W-1] = 1;
    low_differs = value;
    low_differs[0] = 0;
    high_differs = value;
    high_differs[W-1] = 0;
    with_x = value;
    with_x[W/2] = 1'bx;
    with_z = value;
    with_z[W/2] = 1'bz;

    s = '0;
    s.c = value;
    s.tail = 1;
    t = s;
    n = '0;
    n.inner = s;
    n.head = 1;
    u = '0;
    u.as_held = s;
    row = '0;
    row[1] = value;
    row[2] = value;
    i = 2;
    box = new;
    box.h = s;

    // A member of a packed structure, on the left, the right, and both.
    Expect("s.c === value", s.c === value, 1);
    Expect("s.c !== value", s.c !== value, 0);
    Expect("value === s.c", value === s.c, 1);
    Expect("s.c === t.c", s.c === t.c, 1);
    Expect("s.c === low_differs", s.c === low_differs, 0);
    Expect("s.c === high_differs", s.c === high_differs, 0);
    Expect("s.c !== high_differs", s.c !== high_differs, 1);
    Expect("s.c === with_x", s.c === with_x, 0);
    Expect("with_x !== s.c", with_x !== s.c, 1);
    Expect("s.c === with_z", s.c === with_z, 0);

    // A member of a structure held inside another.
    Expect("n.inner.c === value", n.inner.c === value, 1);
    Expect("n.inner.c !== high_differs", n.inner.c !== high_differs, 1);
    Expect("n.inner.c === s.c", n.inner.c === s.c, 1);
    Expect("value === n.inner.c", value === n.inner.c, 1);

    // A member of a structure a packed union holds.
    Expect("u.as_held.c === value", u.as_held.c === value, 1);
    Expect("u.as_held.c === low_differs", u.as_held.c === low_differs, 0);
    Expect("low_differs !== u.as_held.c", low_differs !== u.as_held.c, 1);

    // An element of a packed array, at a constant and a computed index.
    Expect("row[1] === value", row[1] === value, 1);
    Expect("row[1] !== high_differs", row[1] !== high_differs, 1);
    Expect("row[i] === value", row[i] === value, 1);
    Expect("row[i] === row[1]", row[i] === row[1], 1);
    Expect("row[3] === value", row[3] === value, 0);

    // A member of a structure a class property holds, from outside and
    // from the class's own method.
    Expect("box.h.c === value", box.h.c === value, 1);
    Expect("box.h.c !== low_differs", box.h.c !== low_differs, 1);
    Expect("value === box.h.c", value === box.h.c, 1);
    Expect("Box::Same(value)", box.Same(value), 1);
    Expect("Box::Same(with_x)", box.Same(with_x), 0);
    Expect("Box::Differs(high_differs)", box.Differs(high_differs), 1);

    // A function comparing members of the values it is given.
    Expect("SameMembers(s, t)", SameMembers(s, t), 1);
    t.c = high_differs;
    Expect("SameMembers(s, t) once t differs", SameMembers(s, t), 0);

    // A case item's expression.
    case (1'b1)
      (s.c === low_differs): Expect("the low_differs item", 1, 0);
      (s.c === value): Expect("the value item", 1, 1);
      default: Expect("the default item", 1, 0);
    endcase

    // Logical equality and a relation over the same parts.
    Expect("s.c == value", s.c == value, 1);
    Expect("row[i] < value", row[i] < value, 0);
    Expect("high_differs < s.c", high_differs < s.c, 1);

    done = 1;
  end
endmodule

module Top;
  Parts #(.W(64)) at_64 ();
  Parts #(.W(128)) at_128 ();
  Parts #(.W(129)) at_129 ();
  Parts #(.W(200)) at_200 ();

  final begin
    if (at_64.done !== 1 || at_128.done !== 1 || at_129.done !== 1 ||
        at_200.done !== 1)
      $fatal(1, "not every width ran its checks: %b %b %b %b", at_64.done,
             at_128.done, at_129.done, at_200.done);
    if (at_64.checks !== 34 || at_128.checks !== 34 ||
        at_129.checks !== 34 || at_200.checks !== 34)
      $fatal(1, "%0d %0d %0d %0d checks ran, expected 34 each",
             at_64.checks, at_128.checks, at_129.checks, at_200.checks);
    $display("All checks passed");
  end
endmodule
