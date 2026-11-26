// The bit vector functions answer from the bits of the expression they are
// given (LRM 20.9) and `$clog2` from its value (LRM 20.8.1), so an argument
// that is a part of a packed value -- a member of a packed structure or
// union, a member of one held inside another, an element of a packed array at
// a constant or a computed index, a member of a structure a class property
// holds -- answers as the value it holds. That holds at every width, in a
// module's initial procedure, in a function given the value as an argument
// and in a class method. Each part holds its value beside other bits of what
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
    function int Ones();
      return $countones(h.c);
    endfunction
    function bit Unknown();
      return $isunknown(h.c);
    endfunction
  endclass

  function automatic int OnesOfMember(held_t a);
    return $countones(a.c);
  endfunction

  logic [W-1:0] three, top_only, with_x;
  held_t s, one_hot, zero, unknown;
  nest_t n;
  either_t u;
  logic [3:0][W-1:0] row;
  Box box;
  int i;
  int checks;
  bit done;

  task automatic ExpectBit(string what, bit got, bit want);
    if (got !== want)
      $fatal(1, "W=%0d: %s answered %b, expected %b", W, what, got, want);
    checks++;
  endtask

  task automatic ExpectInt(string what, int got, int want);
    if (got !== want)
      $fatal(1, "W=%0d: %s answered %0d, expected %0d", W, what, got, want);
    checks++;
  endtask

  initial begin
    done = 0;
    checks = 0;
    three = '0;
    three[0] = 1;
    three[W/2] = 1;
    three[W-1] = 1;
    top_only = '0;
    top_only[W-1] = 1;
    with_x = three;
    with_x[W/2] = 1'bx;

    s = '1;
    s.c = three;
    one_hot = '1;
    one_hot.c = top_only;
    zero = '1;
    zero.c = '0;
    unknown = '1;
    unknown.c = with_x;
    n = '1;
    n.inner = s;
    u = '1;
    u.as_held = s;
    row = '1;
    row[1] = three;
    row[2] = top_only;
    i = 2;
    box = new;
    box.h = s;

    // A member of a packed structure, which every bit beside it holds a 1.
    ExpectInt("$countones(s.c)", $countones(s.c), 3);
    ExpectInt("$countbits(s.c, '1)", $countbits(s.c, '1), 3);
    ExpectInt("$countbits(s.c, '0)", $countbits(s.c, '0), W - 3);
    ExpectBit("$onehot(s.c)", $onehot(s.c), 0);
    ExpectBit("$onehot(one_hot.c)", $onehot(one_hot.c), 1);
    ExpectBit("$onehot0(zero.c)", $onehot0(zero.c), 1);
    ExpectBit("$onehot0(s.c)", $onehot0(s.c), 0);
    ExpectBit("$isunknown(s.c)", $isunknown(s.c), 0);
    ExpectBit("$isunknown(unknown.c)", $isunknown(unknown.c), 1);
    ExpectInt("$countbits(unknown.c, 'x)", $countbits(unknown.c, 'x), 1);
    ExpectInt("$clog2(s.c)", $clog2(s.c), W);
    ExpectInt("$clog2(one_hot.c)", $clog2(one_hot.c), W - 1);

    // A member of a structure held inside another, and of one a packed
    // union holds.
    ExpectInt("$countones(n.inner.c)", $countones(n.inner.c), 3);
    ExpectBit("$isunknown(n.inner.c)", $isunknown(n.inner.c), 0);
    ExpectInt("$countones(u.as_held.c)", $countones(u.as_held.c), 3);
    ExpectBit("$onehot(u.as_held.c)", $onehot(u.as_held.c), 0);

    // An element of a packed array, at a constant and a computed index.
    ExpectInt("$countones(row[1])", $countones(row[1]), 3);
    ExpectBit("$onehot(row[i])", $onehot(row[i]), 1);
    ExpectInt("$clog2(row[i])", $clog2(row[i]), W - 1);
    ExpectInt("$countones(row[3])", $countones(row[3]), W);

    // A member of a structure a class property holds, from outside and from
    // the class's own method.
    ExpectInt("$countones(box.h.c)", $countones(box.h.c), 3);
    ExpectInt("Box::Ones()", box.Ones(), 3);
    ExpectBit("Box::Unknown()", box.Unknown(), 0);
    box.h = unknown;
    ExpectBit("Box::Unknown() once it holds an x", box.Unknown(), 1);

    // A function counting the bits of a member of the value it is given.
    ExpectInt("OnesOfMember(s)", OnesOfMember(s), 3);
    ExpectInt("OnesOfMember(one_hot)", OnesOfMember(one_hot), 1);

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
    if (at_64.checks !== 26 || at_128.checks !== 26 ||
        at_129.checks !== 26 || at_200.checks !== 26)
      $fatal(1, "%0d %0d %0d %0d checks ran, expected 26 each",
             at_64.checks, at_128.checks, at_129.checks, at_200.checks);
    $display("All checks passed");
  end
endmodule
