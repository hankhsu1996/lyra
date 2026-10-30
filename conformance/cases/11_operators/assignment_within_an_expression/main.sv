// An assignment written inside an expression yields the value it stored:
// the right-hand side cast to the left-hand side's type, stacked, then
// written, and the stacked value is the result -- not the target read again
// afterwards (LRM 11.3.6). So a write the target drops still yields what was
// assigned. A compound assignment yields the value it computed, with the
// target evaluated once (LRM 11.4.1), and a concatenation target yields an
// unsigned value as wide as its operands together.
module Top;
  typedef struct packed {
    logic [3:0] hi;
    logic [3:0] lo;
  } nibbles_t;
  typedef struct {
    int x;
    int y;
  } point_t;

  class Counter;
    int count;
  endclass

  int i, j, k, got;
  logic [3:0] narrow;
  logic [7:0] v, part_got;
  nibbles_t s;
  real r, real_got;
  string str, str_got;
  point_t p, point_got;
  int q[$];
  int calls;
  logic [3:0] a, b;
  logic [7:0] concat_got;
  int taken;
  Counter c;

  function automatic int next_index();
    calls++;
    return 1;
  endfunction

  function automatic int twice_through_a_local(int n);
    int local_copy;
    return 2 * (local_copy = n);
  endfunction

  initial begin
    got = (i = 7);
    if (got !== 7 || i !== 7)
      $fatal(1, "(i = 7) yielded %0d and left i at %0d", got, i);

    got = (i = (j = (k = 3)));
    if (got !== 3 || i !== 3 || j !== 3 || k !== 3)
      $fatal(1, "a chain of assignments yielded %0d (i=%0d j=%0d k=%0d)", got,
             i, j, k);

    got = (narrow = 8'h1f);
    if (got !== 15 || narrow !== 4'hf)
      $fatal(1, "an assignment to 4 bits yielded %0d, expected the cast 15",
             got);

    v = 8'h00;
    part_got = (v[3:0] = 4'h5);
    if (part_got !== 8'h05 || v !== 8'h05)
      $fatal(1, "a slice assignment yielded %h and left %h", part_got, v);
    part_got = (v[7] = 1'b1);
    if (part_got !== 8'h01 || v !== 8'h85)
      $fatal(1, "a bit assignment yielded %h and left %h", part_got, v);
    s = '0;
    part_got = (s.lo = 4'h6);
    if (part_got !== 8'h06 || s !== 8'h06)
      $fatal(1, "a packed member assignment yielded %h and left %h", part_got,
             s);

    real_got = (r = 2.5);
    if (real_got != 2.5 || r != 2.5)
      $fatal(1, "a real assignment yielded %f", real_got);
    str_got = (str = "hi");
    if (str_got != "hi" || str != "hi")
      $fatal(1, "a string assignment yielded \"%s\"", str_got);
    point_got = (p = '{3, 4});
    if (point_got.x !== 3 || point_got.y !== 4 || p.y !== 4)
      $fatal(1, "a struct assignment yielded {%0d, %0d}", point_got.x,
             point_got.y);

    i = 5;
    if ((i = 0)) $fatal(1, "an if took the zero an assignment yielded");
    if (i !== 0) $fatal(1, "the if's assignment left i at %0d", i);
    taken = 0;
    i = 3;
    while ((i = i - 1)) taken++;
    if (taken !== 2 || i !== 0)
      $fatal(1, "a while over an assignment ran %0d times", taken);

    i = 10;
    got = (i += 5);
    if (got !== 15 || i !== 15)
      $fatal(1, "(i += 5) yielded %0d and left i at %0d", got, i);
    q = '{1, 4, 9};
    calls = 0;
    got = (q[next_index()] += 2);
    if (got !== 6 || q[1] !== 6 || calls !== 1)
      $fatal(1, "an indexed += yielded %0d, left %0d, indexed %0d times", got,
             q[1], calls);

    // A write past the end of a queue is dropped (LRM 7.10.1); the value is
    // still the one assigned.
    got = (q[7] = 9);
    if (got !== 9 || q.size() !== 3)
      $fatal(1, "a dropped write yielded %0d (size %0d)", got, q.size());
    got = (q[7] += 2);
    if (got !== 2 || q.size() !== 3)
      $fatal(1, "a dropped += yielded %0d (size %0d)", got, q.size());

    concat_got = ({a, b} = 8'ha5);
    if (concat_got !== 8'ha5 || a !== 4'ha || b !== 4'h5)
      $fatal(1, "a concatenation assignment yielded %h (a=%h b=%h)",
             concat_got, a, b);

    // A loop's initializer and step are assignments whose value nothing
    // reads, a concatenation target among them.
    taken = 0;
    for ({a, b} = 8'h00; a < 2; {a, b} = {a, b} + 8'h10) taken++;
    if (taken !== 2 || a !== 4'h2 || b !== 4'h0)
      $fatal(1, "a loop over a concatenation ran %0d times (a=%h b=%h)",
             taken, a, b);

    c = new;
    got = (c.count = 4);
    if (got !== 4 || c.count !== 4)
      $fatal(1, "a property assignment yielded %0d", got);

    got = twice_through_a_local(21);
    if (got !== 42)
      $fatal(1, "twice an assignment to a function's local gave %0d", got);

    $display("All checks passed");
  end
endmodule
