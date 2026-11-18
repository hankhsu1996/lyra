// An `always_comb` models combinational logic and is triggered when what it
// reads changes (LRM 9.2.2.2, 9.2.2.2.1); an `always_latch` is the same for a
// latch (LRM 9.2.2.3). A procedure that writes a default for an output and
// then its value changes that output twice in one run, and LRM 4.3, 4.5 and
// 4.9.3 make each a change. Whether another procedure reading it runs again is
// left open: LRM 4.7 lets the two procedures' statements interleave, and in a
// schedule where neither is waiting while the other writes, neither sees the
// other's default, while a schedule running each to its stop wakes each with
// the other's default for ever. Lyra runs each procedure to its stop, and
// judges what reaches an `always_comb` or `always_latch` by what each run of
// another process leaves: a value written and written back before that run
// stops is no change to it. So two such procedures each reading the other's
// output settle, as they do in the interleaved schedule. A change that one run
// leaves still triggers it, however many writes made it, and a change one run
// makes and another run undoes still triggers it once, since each run is a
// change of its own; each nonblocking update is one too (LRM 4.6). An event
// control, `@*` among them, keeps the standard's reading, which the
// conformance case on a change undone in the same run holds. This case holds
// Lyra to its choice.
module ForcedReader (
    ref logic in
);
  logic seen;
  always_comb seen = in;
endmodule

module Top;
  // Each reads the other's output, and each writes a default first.
  logic go = 1'b1;
  logic x;
  logic y;
  always_comb begin
    x = 1'b0;
    if (go || y) x = 1'b1;
  end
  always_comb begin
    y = 1'b0;
    if (x) y = 1'b1;
  end

  // Each reads one output of the other, and no bit of either depends on the
  // other's: only the defaults link them.
  logic ack = 1'b1;
  logic req = 1'b1;
  logic evict;
  logic rd_en;
  logic [1:0] stat;
  logic [1:0] buffered;
  always_comb begin
    stat = 2'b00;
    evict = 1'b0;
    if (ack) evict = 1'b1;
    if (rd_en) stat = 2'b01;
  end
  always_comb begin
    buffered = 2'b00;
    rd_en = 1'b0;
    if (evict) buffered = 2'b10;
    if (req) rd_en = 1'b1;
  end

  // The same of values that are no packed vector: an unpacked array cleared
  // whole and then given one element, and a real.
  logic [7:0] onehot[4];
  logic [7:0] answer[4];
  always_comb begin
    onehot = '{default: '0};
    if (go || answer[0][0]) onehot[1] = 8'h01;
  end
  always_comb begin
    answer = '{default: '0};
    if (onehot[1][0]) answer[0] = 8'h01;
  end

  real level;
  real echo;
  always_comb begin
    level = 0.0;
    if (go || echo > 0.5) level = 1.0;
  end
  always_comb begin
    echo = 0.0;
    if (level > 0.5) echo = 1.0;
  end

  // What each counts is read and written by the procedure itself, so it is no
  // part of what triggers it (LRM 9.2.2.2.1).
  logic watched_bit = 1'b0;
  logic bit_seen;
  int bit_runs = 0;
  always_comb begin
    bit_seen = watched_bit;
    bit_runs = bit_runs + 1;
  end

  logic latch_in = 1'b0;
  logic latch_seen;
  int latch_runs = 0;
  always_latch begin
    if (go) latch_seen = latch_in;
    latch_runs = latch_runs + 1;
  end

  int elements[4];
  int element_seen;
  int element_runs = 0;
  always_comb begin
    element_seen = elements[2];
    element_runs = element_runs + 1;
  end

  logic [255:0] wide = '0;
  logic [255:0] wide_seen;
  int wide_runs = 0;
  always_comb begin
    wide_seen = wide;
    wide_runs = wide_runs + 1;
  end

  logic through_ref = 1'b0;
  logic ref_seen;
  int ref_runs = 0;
  always_comb begin
    ref_seen = through_ref;
    ref_runs = ref_runs + 1;
  end

  logic shared = 1'b0;
  logic shared_seen;
  int shared_runs = 0;
  always_comb begin
    shared_seen = shared;
    shared_runs = shared_runs + 1;
  end
  initial #20 shared = 1'b1;
  initial #20 shared = 1'b0;

  logic updated = 1'b0;
  logic updated_seen;
  int updated_runs = 0;
  always_comb begin
    updated_seen = updated;
    updated_runs = updated_runs + 1;
  end

  real watched_real = 0.5;
  real real_seen;
  int real_runs = 0;
  always_comb begin
    real_seen = watched_real;
    real_runs = real_runs + 1;
  end

  logic [7:0] cleared[4] = '{8'h00, 8'h01, 8'h00, 8'h00};
  logic [7:0] cleared_seen;
  int cleared_runs = 0;
  always_comb begin
    cleared_seen = cleared[1];
    cleared_runs = cleared_runs + 1;
  end

  typedef struct {
    logic [3:0] low;
    logic [3:0] high;
  } pair_t;
  pair_t pair = '{4'h1, 4'h2};
  logic [3:0] pair_seen;
  int pair_runs = 0;
  always_comb begin
    pair_seen = pair.high;
    pair_runs = pair_runs + 1;
  end

  int mixed[2];
  logic gate = 1'b0;
  int mixed_seen;
  int mixed_runs = 0;
  always_comb begin
    mixed_seen = mixed[0] + int'(gate);
    mixed_runs = mixed_runs + 1;
  end

  int dynamic[];
  int dynamic_seen;
  int dynamic_runs = 0;
  always_comb begin
    dynamic_seen = dynamic.size() > 1 ? dynamic[1] : -1;
    dynamic_runs = dynamic_runs + 1;
  end

  int line[$] = '{1, 2, 3};
  int line_seen;
  int line_runs = 0;
  always_comb begin
    line_seen = line.size() > 1 ? line[1] : -1;
    line_runs = line_runs + 1;
  end

  int ring[$] = '{1, 2, 3};
  process ring_handle;
  int ring_seen;
  int ring_runs = 0;
  always_comb begin
    if (ring_handle == null) ring_handle = process::self();
    ring_seen = ring.size() > 0 ? ring[0] : -1;
    ring_runs = ring_runs + 1;
  end

  int nested[2][$];
  int nested_seen;
  int nested_runs = 0;
  always_comb begin
    nested_seen = nested[0].size() > 1 ? nested[0][1] : -1;
    nested_runs = nested_runs + 1;
  end

  int lines[2][$];
  process lines_handle;
  int lines_seen;
  int lines_runs = 0;
  always_comb begin
    if (lines_handle == null) lines_handle = process::self();
    lines_seen = lines[0].size() > 1 ? lines[0][1] : -1;
    lines_runs = lines_runs + 1;
  end

  // An associative array's entry is named by its index whatever the index's
  // type, a position's own among them (LRM 7.8).
  int keyed[logic signed [63:0]];
  int keyed_seen;
  always_comb keyed_seen = keyed.exists(5) ? keyed[5] : -1;

  int below = -1;
  int beyond = 5;

  int entries[string];
  int entries_seen;
  int entries_runs = 0;
  always_comb begin
    entries_seen = entries.num();
    entries_runs = entries_runs + 1;
  end

  // A force that a run makes after writing what a procedure reads moves the
  // procedure's wait to the forced value, which then holds what the write
  // left (LRM 10.6.2).
  logic forced_source = 1'b0;
  ForcedReader forced_reader (.in(forced_source));

  // A write past the last element lands where no read reaches (LRM 7.4.5),
  // which is no element and changes nothing.
  int bounded[4];
  int outside = 9;
  int bounded_seen;
  always_comb bounded_seen = bounded[1];
  int last_seen;
  int bounded_runs = 0;
  always_comb begin
    last_seen = bounded[3];
    bounded_runs = bounded_runs + 1;
  end

  logic [7:0] sliced[4] = '{default: '0};
  logic sliced_seen;
  int sliced_runs = 0;
  always_comb begin
    sliced_seen = sliced[1][0];
    sliced_runs = sliced_runs + 1;
  end

  int indexed[8];
  int index = 5;
  int indexed_seen;
  int indexed_runs = 0;
  always_comb begin
    indexed_seen = indexed[3];
    indexed_runs = indexed_runs + 1;
  end

  string text = "";
  string text_seen;
  int text_runs = 0;
  always_comb begin
    text_seen = text;
    text_runs = text_runs + 1;
  end

  int items[$];
  int items_seen;
  int items_runs = 0;
  always_comb begin
    items_seen = items.size();
    items_runs = items_runs + 1;
  end

  // A delayed continuous assignment's update is an event of its own when it
  // matures (LRM 10.3.3).
  logic source = 1'b0;
  logic delayed;
  assign #1 delayed = source;
  logic delayed_seen;
  int delayed_runs = 0;
  always_comb begin
    delayed_seen = delayed;
    delayed_runs = delayed_runs + 1;
  end

  // Stopping a procedure that a run had already reached leaves it runnable, as
  // the change that reached it would have under the standard's reading; one
  // stopped before any change reached it waits afresh once started again (LRM
  // 9.7).
  process now_handle;
  process later_handle;
  process before_handle;
  logic now_in = 1'b0;
  logic later_in = 1'b0;
  logic before_in = 1'b0;
  logic now_seen;
  logic later_seen;
  logic before_seen;
  int now_runs = 0;
  int later_runs = 0;
  int before_runs = 0;
  always_comb begin
    if (now_handle == null) now_handle = process::self();
    now_seen = now_in;
    now_runs = now_runs + 1;
  end
  always_comb begin
    if (later_handle == null) later_handle = process::self();
    later_seen = later_in;
    later_runs = later_runs + 1;
  end
  always_comb begin
    if (before_handle == null) before_handle = process::self();
    before_seen = before_in;
    before_runs = before_runs + 1;
  end
  // One started again in the middle of a run watches from there on and has
  // nothing from the run's start to be measured by, so a write of the run
  // reaching it after that triggers it, as each write would.
  process rearmed_handle;
  logic rearmed_in = 1'b0;
  logic rearmed_seen;
  int rearmed_runs = 0;
  always_comb begin
    if (rearmed_handle == null) rearmed_handle = process::self();
    rearmed_seen = rearmed_in;
    rearmed_runs = rearmed_runs + 1;
  end

  // Two procedures defaulting each other's inputs settle whatever way they
  // write them: through a reference to an element (LRM 13.5.2), or to a
  // member of an unpacked union (LRM 7.3). A class object's properties are no
  // part of either's list (LRM 9.2.2.2.1), so they link no two procedures.
  function automatic void put(ref logic target, input logic value);
    target = 1'b0;
    if (value) target = 1'b1;
  endfunction
  logic lent_a[2];
  logic lent_b[2];
  always_comb put(lent_a[1], go || lent_b[1]);
  always_comb put(lent_b[1], lent_a[1]);

  typedef union {
    logic [7:0] wide;
    logic [3:0] narrow;
  } overlay_t;
  overlay_t overlay_a;
  overlay_t overlay_b;
  always_comb begin
    overlay_a.wide = 8'h00;
    if (go || overlay_b.wide[0]) overlay_a.wide = 8'h01;
  end
  always_comb begin
    overlay_b.wide = 8'h00;
    if (overlay_a.wide[0]) overlay_b.wide = 8'h01;
  end

  // What a run changes is told at the part it wrote, as far as that part is
  // named: of an array whose size changes, of a part nested past what a path
  // follows, and of what a solver writes, the variable or the part reached is
  // what changed.
  int keyed_apart[int];
  int apart_seen;
  int apart_runs = 0;
  always_comb begin
    apart_seen = keyed_apart[1];
    apart_runs = apart_runs + 1;
  end

  int deep[2][2][2][2][2][2][2];
  int deep_seen;
  int deep_runs = 0;
  always_comb begin
    deep_seen = deep[0][0][0][0][0][0][1];
    deep_runs = deep_runs + 1;
  end

  // A queue nested deeper than a path follows, written through a reference and
  // beside it in one run.
  int deep_lines[2][2][2][2][2][2][2][$];
  int deep_lines_seen;
  int deep_lines_runs = 0;
  always_comb begin
    deep_lines_seen = deep_lines[0][0][0][0][0][0][1][0];
    deep_lines_runs = deep_lines_runs + 1;
  end

  int tail[$] = '{1, 2, 3};
  int tail_seen;
  int tail_runs = 0;
  always_comb begin
    tail_seen = tail[1];
    tail_runs = tail_runs + 1;
  end

  int held[$] = '{1, 2, 3};
  int held_seen;
  int held_runs = 0;
  always_comb begin
    held_seen = held[1];
    held_runs = held_runs + 1;
  end

  // A memory declared packed, as older Verilog had to, is written by element
  // and read by an element a constant names or by one an index chooses (LRM
  // 7.4.1, 11.5.3).
  logic [255:0][7:0] packed_mem;
  logic [7:0] packed_index = 8'd3;
  logic [7:0] packed_fixed_seen;
  logic [7:0] packed_any_seen;
  int packed_fixed_runs = 0;
  int packed_any_runs = 0;
  always_comb begin
    packed_fixed_seen = packed_mem[3];
    packed_fixed_runs = packed_fixed_runs + 1;
  end
  always_comb begin
    packed_any_seen = packed_mem[packed_index];
    packed_any_runs = packed_any_runs + 1;
  end

  int chosen[4];
  int chosen_seen;
  int chosen_runs = 0;
  always_comb begin
    chosen_seen = chosen[1];
    chosen_runs = chosen_runs + 1;
  end

  task automatic write_back(ref int target);
    int was;
    was = target;
    target = was + 1;
    target = was;
  endtask

  task automatic write_and_restore(ref logic target);
    target = 1'b1;
    target = 1'b0;
  endtask

  task automatic bind_only(ref int entry);
  endtask

  task automatic write_then_resize(ref int entry);
    entry = 9;
    dynamic = new[8](dynamic);
    entry = 0;
  endtask

  task automatic resize_then_write_later(ref int entry);
    dynamic = new[2];
    #1;
    entry = 5;
  endtask

  task automatic grow_front_then_write(ref int entry);
    items.push_front(9);
    entry = 5;
  endtask

  task automatic clear_line(ref int line_of[$]);
    line_of.delete();
  endtask

  task automatic expect_runs(input string what, input int got,
                             input int want);
    if (got !== want)
      $fatal(1, "%s: the procedure ran %0d times, expected %0d", what, got,
             want);
  endtask

  initial begin
    int base;
    int base2;
    #1;
    if (x !== 1'b1 || y !== 1'b1)
      $fatal(1, "the procedures reading each other settled at x=%b y=%b", x, y);
    if (evict !== 1'b1 || rd_en !== 1'b1 || stat !== 2'b01 ||
        buffered !== 2'b10)
      $fatal(1, "the linked procedures settled at evict=%b rd_en=%b", evict,
             rd_en);
    ack = 1'b0;
    #1;
    if (evict !== 1'b0 || buffered !== 2'b00 || stat !== 2'b01)
      $fatal(1, "an input change left evict=%b buffered=%b", evict, buffered);
    ack = 1'b1;
    #1;
    if (evict !== 1'b1 || buffered !== 2'b10)
      $fatal(1, "an input restored left evict=%b buffered=%b", evict,
             buffered);
    if (onehot[1] !== 8'h01 || answer[0] !== 8'h01)
      $fatal(1, "the array procedures settled at onehot[1]=%h answer[0]=%h",
             onehot[1], answer[0]);
    if (level != 1.0 || echo != 1.0)
      $fatal(1, "the real procedures settled at level=%f echo=%f", level, echo);

    base = bit_runs;
    watched_bit = 1'b1;
    watched_bit = 1'b0;
    #1 expect_runs("a bit written and written back", bit_runs - base, 0);
    base = bit_runs;
    watched_bit = 1'b1;
    watched_bit = 1'b0;
    watched_bit = 1'b1;
    #1 expect_runs("a bit a run leaves changed", bit_runs - base, 1);
    if (bit_seen !== 1'b1) $fatal(1, "the procedure saw %b, expected 1", bit_seen);

    base = latch_runs;
    latch_in = 1'b1;
    latch_in = 1'b0;
    #1 expect_runs("a latch input written back", latch_runs - base, 0);

    base = element_runs;
    elements[2] = 9;
    elements[2] = 0;
    #1 expect_runs("an element written back", element_runs - base, 0);
    base = element_runs;
    elements[2] = 9;
    elements[2] = 7;
    #1 expect_runs("an element a run leaves changed", element_runs - base, 1);

    base = wide_runs;
    wide[3:0] = 4'hf;
    wide[255:252] = 4'hf;
    wide[3:0] = 4'h0;
    wide[255:252] = 4'h0;
    #1 expect_runs("both ends of a wide value written back", wide_runs - base,
                   0);
    base = wide_runs;
    wide[3:0] = 4'hf;
    wide[255:252] = 4'hf;
    wide[3:0] = 4'h0;
    #1 expect_runs("one end of a wide value left changed", wide_runs - base,
                   1);

    base = ref_runs;
    write_and_restore(through_ref);
    #1 expect_runs("a variable written back through a reference",
                   ref_runs - base, 0);

    // Now 11: both writes are made at 20.
    #5 base = shared_runs;
    #5 expect_runs("a change one run made and another undid",
                   shared_runs - base, 1);

    base = updated_runs;
    updated <= 1'b1;
    updated <= 1'b0;
    #1 expect_runs("two nonblocking updates undoing each other",
                   updated_runs - base, 1);

    base = real_runs;
    watched_real = 2.5;
    watched_real = 0.5;
    #1 expect_runs("a real written back", real_runs - base, 0);
    base = real_runs;
    watched_real = 2.5;
    #1 expect_runs("a real a run leaves changed", real_runs - base, 1);

    base = cleared_runs;
    cleared = '{default: '0};
    cleared[1] = 8'h01;
    #1 expect_runs("an array cleared whole and its element written back",
                   cleared_runs - base, 0);
    base = cleared_runs;
    cleared[1] = 8'h07;
    cleared = '{default: '0};
    cleared[1] = 8'h01;
    #1 expect_runs("an element written, then the array cleared and restored",
                   cleared_runs - base, 0);

    base = pair_runs;
    pair = '{default: '0};
    pair.low = 4'h1;
    pair.high = 4'h2;
    #1 expect_runs("a structure cleared whole and its members written back",
                   pair_runs - base, 0);
    base = pair_runs;
    pair.low = 4'h9;
    #1 expect_runs("a member the procedure does not read changed",
                   pair_runs - base, 0);
    base = pair_runs;
    pair.high = 4'hf;
    pair.high = 4'h2;
    pair.low = 4'h3;
    #1 expect_runs("the member read written back beside one that changed",
                   pair_runs - base, 0);

    base = mixed_runs;
    gate = 1'b1;
    gate = 1'b0;
    mixed[1] = 5;
    #1 expect_runs("a variable read written back, an element not read changed",
                   mixed_runs - base, 0);

    dynamic = new[4];
    #1 base = dynamic_runs;
    dynamic[1] = 5;
    dynamic[1] = 0;
    #1 expect_runs("a dynamic array's element written back",
                   dynamic_runs - base, 0);
    base = dynamic_runs;
    dynamic[1] = 5;
    #1 expect_runs("a dynamic array's element a run leaves changed",
                   dynamic_runs - base, 1);
    if (dynamic_seen != 5)
      $fatal(1, "the procedure saw %0d of the element", dynamic_seen);
    base = dynamic_runs;
    dynamic[1] = 9;
    dynamic = '{0, 9, 0, 7};
    dynamic = '{0, 5, 0, 0};
    #1 expect_runs("an element written, then the array written whole and back",
                   dynamic_runs - base, 0);
    // A reference into a dynamic array names no position in it, since it goes
    // on naming its element after the array is resized (LRM 13.5.2), so a
    // write through it keeps nothing and is judged by itself. An outdated
    // reference's element lasts only until its call returns, which can be
    // before the event's execution that wrote it ends. The resize wakes the
    // procedure, and so does the later write, judged by itself, though it
    // lands outside the array.
    base = dynamic_runs;
    write_back(dynamic[1]);
    #1 expect_runs("a dynamic array's element written back by reference",
                   dynamic_runs - base, 1);
    base = dynamic_runs;
    write_then_resize(dynamic[1]);
    if (dynamic.size() != 8 || dynamic[1] != 9)
      $fatal(1, "a write by reference, then a resize, left %p", dynamic);
    #1 expect_runs("an element written by reference, then the array resized",
                   dynamic_runs - base, 1);
    base = dynamic_runs;
    resize_then_write_later(dynamic[1]);
    if (dynamic.size() != 2)
      $fatal(1, "a write by an outdated reference left %p", dynamic);
    #1 expect_runs("an outdated reference written in a later execution",
                   dynamic_runs - base, 2);

    base = line_runs;
    line[1] = 9;
    line.push_front(0);
    line[2] = 2;
    void'(line.pop_front());
    #1 expect_runs("an element written, the queue grown and shrunk, it back",
                   line_runs - base, 0);

    ring_handle.suspend();
    #1 base = ring_runs;
    ring[0] = 5;
    ring_handle.resume();
    ring.delete();
    ring = '{1, 2, 3};
    #1 expect_runs("started again, then the queue cleared and filled again",
                   ring_runs - base, 1);

    nested[0] = '{1, 2, 3};
    #1 base = nested_runs;
    nested[0][1] = 7;
    clear_line(nested[0]);
    #1 expect_runs("an element written, then its queue cleared through a reference",
                   nested_runs - base, 1);
    if (nested_seen != -1)
      $fatal(1, "the procedure saw %0d of the cleared queue", nested_seen);

    lines[0] = '{1, 2};
    #1 lines_handle.suspend();
    #1 base = lines_runs;
    lines[0][1] = 7;
    clear_line(lines[0]);
    lines_handle.resume();
    lines[0] = '{9};
    lines[0] = '{};
    // Started again in the middle of a run, it has nothing from the run's
    // start to be measured by, so a write reaching it wakes it.
    #1 expect_runs("started again after its queue was cleared through a reference",
                   lines_runs - base, 1);

    base = line_runs;
    line[0:1] = '{7, 8};
    line[0:1] = '{1, 2};
    #1 expect_runs("a slice of a queue written and written back",
                   line_runs - base, 0);
    base = line_runs;
    line[below:1] = '{7, 8};
    line[1:beyond] = '{9, 9};
    line[below:beyond] = '{1, 2, 3};
    #1 expect_runs("a queue slice reaching outside the queue written back",
                   line_runs - base, 0);

    keyed[5] = 1;
    #1 keyed[5] = 2;
    #1 if (keyed_seen != 2)
      $fatal(1, "an entry of an array indexed by a position saw %0d",
             keyed_seen);

    base = entries_runs;
    bind_only(entries["made"]);
    entries.delete("made");
    #1 expect_runs("an entry made by binding it and deleted",
                   entries_runs - base, 0);

    bounded[outside] = 4;
    bounded[1] = 5;
    #1 if (bounded_seen != 5)
      $fatal(1, "an element written after a write outside the array: saw %0d",
             bounded_seen);
    bounded[1] = 6;
    bounded[outside] = 4;
    #1 if (bounded_seen != 6)
      $fatal(1, "an element written before a write outside the array: saw %0d",
             bounded_seen);

    base = sliced_runs;
    sliced[0:1] = '{8'h05, 8'h05};
    sliced[0:1] = '{8'h00, 8'h00};
    #1 expect_runs("a slice written and written back", sliced_runs - base, 0);
    base = sliced_runs;
    sliced[0:1] = '{sliced[0], sliced[1]};
    sliced[1][0] = 1'b1;
    sliced[1][0] = 1'b0;
    sliced[1][7] = 1'b1;
    #1 expect_runs("a bit read written back after a slice kept its element",
                   sliced_runs - base, 0);
    base = sliced_runs;
    sliced[0:1] = '{8'h00, 8'h01};
    #1 expect_runs("a slice leaving the bit read changed", sliced_runs - base,
                   1);
    if (sliced_seen !== 1'b1)
      $fatal(1, "the procedure saw %b of the sliced element", sliced_seen);
    base = bounded_runs;
    bounded[outside - 6 +: 2] = '{5, 6};
    bounded[outside - 6 +: 2] = '{0, 0};
    #1 expect_runs("a slice reaching past the last element written back",
                   bounded_runs - base, 0);

    forced_source = 1'b1;
    force forced_reader.in = 1'b1;
    #1 if (forced_reader.seen !== 1'b1)
      $fatal(1, "a procedure whose input was written and then forced saw %b",
             forced_reader.seen);
    release forced_reader.in;

    index = 3;
    #1 base = indexed_runs;
    indexed[index] = 4;
    indexed[index] = 0;
    #1 expect_runs("an element written back through a variable index",
                   indexed_runs - base, 0);

    base = text_runs;
    text = "changed";
    text = "";
    #1 expect_runs("a string written back", text_runs - base, 0);

    base = items_runs;
    items.push_back(1);
    items.pop_back();
    #1 expect_runs("a queue grown and shrunk back", items_runs - base, 0);
    base = items_runs;
    items.push_back(1);
    items.push_back(2);
    void'(items.pop_back());
    #1 expect_runs("a queue a run leaves grown", items_runs - base, 1);
    if (items_seen != 1)
      $fatal(1, "the procedure saw %0d elements of the grown queue",
             items_seen);
    base = items_runs;
    items[$+1] = 7;
    void'(items.pop_back());
    #1 expect_runs("a queue appended to by index and shrunk back",
                   items_runs - base, 0);
    base = items_runs;
    items.push_back(5);
    items.push_front(0);
    void'(items.pop_front());
    #1 expect_runs("a queue grown at its end, then changed at its front",
                   items_runs - base, 1);
    base = items_runs;
    void'(items.pop_back());
    items.push_front(9);
    items.delete(0);
    #1 expect_runs("a queue shrunk at its end, then changed at its front",
                   items_runs - base, 1);
    if (items_seen != 1)
      $fatal(1, "the procedure saw %0d elements of the queue", items_seen);
    base = items_runs;
    items.push_back(2);
    items.push_back(3);
    items.push_front(0);
    void'(items.pop_front());
    if (items.size() != 3 || items[0] != 1 || items[1] != 2 || items[2] != 3)
      $fatal(1, "a queue grown twice was left %p once kept whole", items);
    items = '{1, 2};
    #1 expect_runs("a queue grown twice at its end, then assigned whole",
                   items_runs - base, 1);
    base = items_runs;
    void'(items.pop_back());
    void'(items.pop_back());
    items.push_front(0);
    void'(items.pop_front());
    if (items.size() != 0)
      $fatal(1, "a queue shrunk twice was left %p once kept whole", items);
    items = '{1};
    #1 expect_runs("a queue shrunk twice at its end, then assigned whole",
                   items_runs - base, 1);
    if (items_seen != 1)
      $fatal(1, "the procedure saw %0d elements of the queue", items_seen);
    // An element a call reaches by reference stays the one it named while the
    // queue grows at its front (LRM 13.5.2, 7.10.3).
    base = items_runs;
    items.push_back(4);
    grow_front_then_write(items[1]);
    if (items.size() != 3 || items[0] != 9 || items[1] != 1 || items[2] != 5)
      $fatal(1, "a write by reference after a push at the front left %p",
             items);
    #1 expect_runs("a queue grown at its end, then grown at its front",
                   items_runs - base, 1);

    base = delayed_runs;
    source = 1'b1;
    #2 expect_runs("a delayed assignment maturing", delayed_runs - base, 1);
    if (delayed_seen !== 1'b1)
      $fatal(1, "the procedure saw %b of the delayed update", delayed_seen);

    base = now_runs;
    now_in = 1'b1;
    now_handle.suspend();
    now_in = 1'b0;
    now_handle.resume();
    #1 expect_runs("stopped and started in the run that reached it",
                   now_runs - base, 1);
    base = later_runs;
    later_in = 1'b1;
    later_handle.suspend();
    later_in = 1'b0;
    #1 expect_runs("stopped by the run that reached it", later_runs - base, 0);
    later_handle.resume();
    #1 expect_runs("started again a time later", later_runs - base, 1);
    base = before_runs;
    before_handle.suspend();
    before_in = 1'b1;
    before_in = 1'b0;
    before_handle.resume();
    #1 expect_runs("stopped before a change reached it", before_runs - base, 0);
    rearmed_handle.suspend();
    #1 base = rearmed_runs;
    rearmed_in = 1'b1;
    rearmed_handle.resume();
    rearmed_in = 1'b0;
    #1 expect_runs("started again between a write and its undoing",
                   rearmed_runs - base, 1);
    if (rearmed_seen !== 1'b0)
      $fatal(1, "the procedure started again saw %b", rearmed_seen);
    base = rearmed_runs;
    rearmed_in = 1'b1;
    rearmed_handle.suspend();
    rearmed_handle.resume();
    rearmed_in = 1'b1;
    #1 expect_runs("started again by the run that reached it, then left as it was",
                   rearmed_runs - base, 1);

    base = rearmed_runs;
    rearmed_handle.suspend();
    #1 rearmed_in = 1'b0;
    rearmed_handle.resume();
    rearmed_in = 1'b1;
    rearmed_in = 1'b0;
    #1 expect_runs("started again after a write, then written and written back",
                   rearmed_runs - base, 1);

    if (lent_a[1] !== 1'b1 || lent_b[1] !== 1'b1)
      $fatal(1, "the procedures writing through references settled at %b %b",
             lent_a[1], lent_b[1]);
    if (overlay_a.wide !== 8'h01 || overlay_b.wide !== 8'h01)
      $fatal(1, "the procedures writing union members settled at %h %h",
             overlay_a.wide, overlay_b.wide);

    // A part written whole is compared whole, so a change anywhere in it wakes
    // a reader of any of it, whose run then changes nothing.
    base = pair_runs;
    pair = '{4'h9, 4'h2};
    #1 expect_runs("a structure written whole, the member read unchanged",
                   pair_runs - base, 1);
    base = cleared_runs;
    cleared = '{8'h00, 8'h01, 8'h05, 8'h00};
    #1 expect_runs("an array written whole, the element read unchanged",
                   cleared_runs - base, 1);

    // An array whose size changes is read and kept whole, so a change to any of
    // its entries is a change of what the procedure reads.
    keyed_apart[1] = 4;
    #1 base = apart_runs;
    keyed_apart[2] = 5;
    #1 expect_runs("an entry the procedure does not read made",
                   apart_runs - base, 1);
    base = apart_runs;
    keyed_apart[1] = 6;
    #1 expect_runs("the entry the procedure reads changed", apart_runs - base,
                   1);
    if (apart_seen != 6)
      $fatal(1, "the procedure saw %0d of the entry", apart_seen);

    // A part nested deeper than a path follows is named by the part the path
    // reaches, so a write beside the one read is a change of it.
    base = deep_runs;
    deep[0][0][0][0][0][0][0] = 3;
    #1 expect_runs("a deep element beside the one read changed",
                   deep_runs - base, 1);

    deep_lines[0][0][0][0][0][0][0] = '{1};
    deep_lines[0][0][0][0][0][0][1] = '{1};
    #1 base = deep_lines_runs;
    write_back(deep_lines[0][0][0][0][0][0][0][0]);
    deep_lines[0][0][0][0][0][0][1][0] = 5;
    #1 expect_runs("a deep queue written back through a reference, its neighbour changed",
                   deep_lines_runs - base, 1);

    base = tail_runs;
    tail.push_back(4);
    #1 expect_runs("a queue appended to, the element read unchanged",
                   tail_runs - base, 1);

    // A reference into a queue names no position in it, so a write through it
    // keeps nothing and is judged by itself.
    base = held_runs;
    write_back(held[1]);
    #1 expect_runs("a queue element written back through a reference",
                   held_runs - base, 1);

    packed_mem = '0;
    #1 base = packed_fixed_runs;
    base2 = packed_any_runs;
    packed_mem[200] = 8'h11;
    #1 expect_runs("a packed memory's element the constant does not name changed",
                   packed_fixed_runs - base, 0);
    expect_runs("a packed memory's element changed, read by an index",
                packed_any_runs - base2, 1);
    base = packed_fixed_runs;
    base2 = packed_any_runs;
    packed_mem[3] = 8'h22;
    packed_mem[250] = 8'h33;
    packed_mem[3] = 8'h00;
    packed_mem[250] = 8'h00;
    #1 expect_runs("two far elements of a packed memory written and written back",
                   packed_any_runs - base2, 0);
    expect_runs("the element read written and written back beside a far one",
                packed_fixed_runs - base, 0);
    base = packed_fixed_runs;
    packed_mem[250] = 8'h44;
    packed_mem[3] = 8'h55;
    #1 expect_runs("a packed memory's element read changed after a far one",
                   packed_fixed_runs - base, 1);
    if (packed_fixed_seen !== 8'h55)
      $fatal(1, "the procedure saw %h of the packed element", packed_fixed_seen);

    chosen[1] = 5;
    #1 base = chosen_runs;
    if (std::randomize(chosen) with {
          chosen[1] == 5;
          chosen[0] == 7;
        } == 0)
      $fatal(1, "the solver found no values for the array");
    // What a solver writes keeps nothing, so its writing the variable decides.
    #1 expect_runs("an array a solver wrote, the element read left as it was",
                   chosen_runs - base, 1);

    $display("All checks passed");
  end
endmodule
