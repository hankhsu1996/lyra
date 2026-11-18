// `vpi_put_value` with no delay writes a variable, or a part of one, as the
// design's own write of it would (LRM 38.34): in every format of LRM Table 38-3
// a value is written in, from foreign code running inside a process's call.
// What waits on the variable, or on the part written, wakes; what waits on
// another part of it does not, and neither does anything when the value written
// is the one it held (LRM 9.4.2); a combinational block reading it runs again
// (LRM 9.2.2.2.1). A part reached through several steps -- a member of an
// element of an array a structure holds, a member of a structure a union holds
// -- is written as the design's own write of it is, the same waits waking as
// for a write through a reference to it (LRM 13.5.2). An element of an
// associative array the write names is made by the write, and a read of one it
// does not hold makes nothing (LRM 7.8.6). A handle to an element of a dynamic
// array names whatever the array holds there when it is used (LRM 37.17 detail
// 27), so one kept across a resize reads the new element. A variable declared
// `const` is not written (LRM 6.20.6), nor is an object with no value, nor a
// vector a value with strength (LRM 38.34), and each is the routine's error.
module Top;
  import "DPI-C" context function int write_all();
  import "DPI-C" context function int write_again();

  typedef struct packed {
    bit [3:0] hi;
    bit [3:0] lo;
  } nibbles_t;
  typedef struct {
    int m;
    int n;
  } inner_t;
  typedef struct {
    inner_t row[2];
  } outer_t;
  typedef union {
    inner_t pair;
    longint whole;
  } over_t;
  typedef union tagged {
    int number;
    real fraction;
  } either_t;

  logic [7:0] lv;
  logic [3:0] lx;
  int i = 1;
  real r;
  string s;
  nibbles_t ps = 8'h35;
  int ua[0:3];
  int da[];
  int aa[int];
  time t;
  const int fixed = 4;
  outer_t deep;
  over_t over;
  either_t held = tagged number 3;

  int woke_lv = 0;
  int woke_i = 0;
  int woke_hi = 0;
  int woke_lo = 0;
  logic [7:0] doubled;
  int woke_m = 0;
  int woke_n = 0;

  always @(lv) woke_lv++;
  always @(i) woke_i++;
  always @(ps.hi) woke_hi++;
  always @(ps.lo) woke_lo++;
  always_comb doubled = lv << 1;
  always @(deep.row[1].m) woke_m++;
  always @(deep.row[1].n) woke_n++;

  task automatic put(ref int place, input int value);
    place = value;
  endtask

  int differences = -1;

  initial begin
    da = new[2];
    #1 differences = write_all();
    if (da[1] !== 4) $fatal(1, "da[1] is %0d, expected 4", da[1]);
    da = new[4];
    da[1] = 21;
    #1 differences += write_again();
    #1;
    if (lv !== 8'h21) $fatal(1, "lv is %h, expected 21", lv);
    if (doubled !== 8'h42) $fatal(1, "lv << 1 is %h, expected 42", doubled);
    if (woke_lv !== 1) $fatal(1, "a wait on lv woke %0d times", woke_lv);
    if (lx !== 4'b1x0z) $fatal(1, "lx is %b, expected 1x0z", lx);
    if (i !== -7) $fatal(1, "i is %0d, expected -7", i);
    if (woke_i !== 1)
      $fatal(1, "a wait on i woke %0d times, expected once", woke_i);
    if (r != 1.5) $fatal(1, "r is %f, expected 1.5", r);
    if (s != "yo") $fatal(1, "s is %s, expected yo", s);
    if (ps !== 8'hC5) $fatal(1, "ps is %h, expected c5", ps);
    if (woke_hi !== 1 || woke_lo !== 0)
      $fatal(1, "waits on ps woke %0d and %0d times", woke_hi, woke_lo);
    if (ua[2] !== 9) $fatal(1, "ua[2] is %0d, expected 9", ua[2]);
    if (!aa.exists(7) || aa[7] !== 8) $fatal(1, "aa[7] was not written");
    if (aa.exists(11)) $fatal(1, "a read of aa[11] made it");
    if (t !== 64'h2_0000_0003) $fatal(1, "t is %h", t);
    if (fixed !== 4) $fatal(1, "a const variable was written");
    if (deep.row[1].m !== 5) $fatal(1, "deep.row[1].m is %0d", deep.row[1].m);
    if (over.pair.n !== 8) $fatal(1, "over.pair.n is %0d", over.pair.n);
    if (held.number !== 11) $fatal(1, "held.number is %0d", held.number);
    if (woke_m !== 1 || woke_n !== 0)
      $fatal(1, "a write of deep.row[1].m woke %0d and %0d", woke_m, woke_n);
    put(deep.row[1].m, 6);
    #1;
    if (woke_m !== 2 || woke_n !== 0)
      $fatal(1, "a write through a reference woke %0d and %0d", woke_m,
             woke_n);
  end

  final begin
    if (differences !== 0)
      $fatal(1, "the writes differ in %0d answers", differences);
    $display("All checks passed");
  end
endmodule
