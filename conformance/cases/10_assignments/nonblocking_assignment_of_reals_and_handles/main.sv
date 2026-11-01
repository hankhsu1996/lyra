// A nonblocking assignment evaluates its right-hand side when the statement
// executes and updates its target only at the end of the time step (LRM
// 10.4.2), whatever the target's type: a `real` or `shortreal` (LRM 6.12), a
// `chandle` (LRM 6.14), a virtual interface (LRM 25.9), or a structure holding
// a real and a chandle. The target reads its old value for the rest of the
// step, and what lands is what the right-hand side was when the statement ran,
// not what its operands hold when the update is applied.
interface Port;
  int tag;
endinterface

module Top;
  import "DPI-C" function chandle allocate_cell(input int seed);
  import "DPI-C" function int read_cell(input chandle handle);
  import "DPI-C" function void release_cell(input chandle handle);

  typedef struct {
    real    weight;
    chandle held;
  } Entry;

  Port first ();
  Port second ();

  real         r;
  shortreal    s;
  chandle      c;
  virtual Port p;
  Entry        e;

  real         r_source;
  chandle      c_source;
  virtual Port p_source;

  real         r_before;
  shortreal    s_before;
  bit          c_was_null_before;
  bit          p_was_first_before;
  real         e_weight_before;
  bit          e_held_was_null_before;

  chandle      one;
  chandle      two;

  initial begin
    first.tag = 1;
    second.tag = 2;
    one = allocate_cell(11);
    two = allocate_cell(22);

    r = 0.5;
    s = 0.25;
    c = null;
    p = first;
    e = '{weight: 0.125, held: null};

    r_source = 2.75;
    c_source = one;
    p_source = second;

    r <= r_source * 2.0;
    s <= 1.5;
    c <= c_source;
    p <= p_source;
    e <= '{weight: r_source, held: c_source};

    // The right-hand sides were evaluated above; what they read changes now.
    r_source = -1.0;
    c_source = two;
    p_source = first;

    r_before = r;
    s_before = s;
    c_was_null_before = (c === null);
    p_was_first_before = (p == first);
    e_weight_before = e.weight;
    e_held_was_null_before = (e.held === null);

    #1;
  end

  final begin
    if (r_before !== 0.5) $fatal(1, "r read %f before the update, expected 0.5", r_before);
    if (s_before !== 0.25) $fatal(1, "s read %f before the update, expected 0.25", s_before);
    if (c_was_null_before !== 1'b1) $fatal(1, "c was not null before the update");
    if (p_was_first_before !== 1'b1) $fatal(1, "p was not first before the update");
    if (e_weight_before !== 0.125)
      $fatal(1, "e.weight read %f before the update, expected 0.125", e_weight_before);
    if (e_held_was_null_before !== 1'b1) $fatal(1, "e.held was not null before the update");

    if (r !== 5.5) $fatal(1, "r was %f, expected 5.5", r);
    if (s !== 1.5) $fatal(1, "s was %f, expected 1.5", s);
    if (c !== one) $fatal(1, "c was not the chandle the right-hand side read");
    if (read_cell(c) !== 11) $fatal(1, "c reached a cell holding %0d, expected 11", read_cell(c));
    if (p != second) $fatal(1, "p was not the instance the right-hand side read");
    if (p.tag !== 2) $fatal(1, "p reached tag %0d, expected 2", p.tag);
    if (e.weight !== 2.75) $fatal(1, "e.weight was %f, expected 2.75", e.weight);
    if (e.held !== one) $fatal(1, "e.held was not the chandle the right-hand side read");

    release_cell(one);
    release_cell(two);
    $display("All checks passed");
  end
endmodule
