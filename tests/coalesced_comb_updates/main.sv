// What a design built with `--coalesce-comb-updates` does. An `always_comb`
// models combinational logic and is triggered when what it reads changes (LRM
// 9.2.2.2, 9.2.2.2.1); an `always_latch` is the same for a latch (LRM
// 9.2.2.3). A procedure that writes a default for an output and then its value
// changes that output twice in one execution, and LRM 4.3, 4.5 and 4.9.3 make
// each a change, so two such procedures reading each other's outputs wake each
// other for ever in a schedule running each to its stop. Built with the option,
// the updates one such procedure's execution makes to what another reads are
// coalesced: the reader is triggered by the values the execution leaves, and
// a value written and written back is no change to it, whatever its type and
// however the write reached it. So such procedures settle. A write by any other
// process -- an `initial`, a nonblocking update, a continuous assignment
// maturing -- triggers it as the standard reads, which a design built without
// the option does for every write.
module ForcedReader (
    input logic in
);
  logic seen;
  int runs = 0;
  always_comb begin
    seen = in;
    runs = runs + 1;
  end
endmodule

class Token;
  int id;
  function new(int id);
    this.id = id;
  endfunction
endclass

module Top;
  logic go = 1'b1;

  // Each reads the other's output, and each writes a default first.
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
  // whole and then given one element, a real, a string.
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

  string said;
  string heard;
  always_comb begin
    said = "";
    if (go || heard == "y") said = "x";
  end
  always_comb begin
    heard = "";
    if (said == "x") heard = "y";
  end

  // Whatever way the procedures write them: through a reference to an element
  // (LRM 13.5.2), or to a member of an unpacked union (LRM 7.3).
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

  // Each procedure below reads `poke`, so every change of it runs them all;
  // each writes a glitch into what it owns and then what its level says, and
  // a procedure reading that runs only where the level moved.
  int poke = 0;

  logic bit_level = 1'b0;
  logic bit_out;
  logic bit_seen;
  int bit_runs = 0;
  always_comb begin
    bit_out = ~bit_level ^ poke[0];
    bit_out = bit_level;
  end
  always_comb begin
    bit_seen = bit_out;
    bit_runs = bit_runs + 1;
  end

  logic latch_level = 1'b0;
  logic latch_out;
  logic latch_seen;
  int latch_runs = 0;
  always_latch begin
    if (go) begin
      latch_out = ~latch_level ^ poke[0];
      latch_out = latch_level;
    end
  end
  always_latch begin
    if (go) latch_seen = latch_out;
    latch_runs = latch_runs + 1;
  end

  int element_level = 0;
  int elements[4];
  int element_seen;
  int element_runs = 0;
  always_comb begin
    elements[2] = poke + 100;
    elements[2] = element_level;
  end
  always_comb begin
    element_seen = elements[2];
    element_runs = element_runs + 1;
  end

  logic [255:0] wide_level = '0;
  logic [255:0] wide_out;
  logic [255:0] wide_seen;
  int wide_runs = 0;
  always_comb begin
    wide_out = wide_level;
    wide_out[3:0] = ~wide_level[3:0] ^ 4'(poke);
    wide_out[255:252] = ~wide_level[255:252];
    wide_out[3:0] = wide_level[3:0];
    wide_out[255:252] = wide_level[255:252];
  end
  always_comb begin
    wide_seen = wide_out;
    wide_runs = wide_runs + 1;
  end

  logic ref_level = 1'b0;
  logic ref_out;
  logic ref_seen;
  int ref_runs = 0;
  function automatic void glitch(ref logic target, input logic value,
                                 input int unused);
    target = ~value;
    target = value;
  endfunction
  always_comb glitch(ref_out, ref_level, poke);
  always_comb begin
    ref_seen = ref_out;
    ref_runs = ref_runs + 1;
  end

  real real_level = 0.5;
  real real_out;
  real real_seen;
  int real_runs = 0;
  always_comb begin
    real_out = real_level + 1.0 + real'(poke);
    real_out = real_level;
  end
  always_comb begin
    real_seen = real_out;
    real_runs = real_runs + 1;
  end

  // An array cleared whole and its element written: the whole is what the
  // execution keeps first, and the element after it lies inside it.
  logic [7:0] cleared_level = 8'h01;
  logic [7:0] cleared[4];
  logic [7:0] cleared_seen;
  int cleared_runs = 0;
  always_comb begin
    cleared = '{default: 8'(poke)};
    cleared = '{default: '0};
    cleared[1] = cleared_level;
  end
  always_comb begin
    cleared_seen = cleared[1];
    cleared_runs = cleared_runs + 1;
  end

  // The element first, then the whole: what the execution keeps of the whole is
  // the array as it stood before the execution, the element's own write put
  // back.
  logic [7:0] refilled_level = 8'h01;
  logic [7:0] refilled[4];
  logic [7:0] refilled_seen;
  int refilled_runs = 0;
  always_comb begin
    refilled[1] = 8'h07 ^ 8'(poke);
    refilled = '{default: '0};
    refilled[1] = refilled_level;
  end
  always_comb begin
    refilled_seen = refilled[1];
    refilled_runs = refilled_runs + 1;
  end

  typedef struct {
    logic [3:0] low;
    logic [3:0] high;
  } pair_t;
  logic [3:0] pair_level = 4'h2;
  pair_t pair;
  logic [3:0] pair_seen;
  int pair_runs = 0;
  always_comb begin
    pair = '{default: 4'(poke)};
    pair.low = 4'h1;
    pair.high = pair_level;
  end
  always_comb begin
    pair_seen = pair.high;
    pair_runs = pair_runs + 1;
  end

  // Only its members written: a member the procedure does not read changing
  // reaches it no further than that member.
  logic [3:0] low_level = 4'h1;
  pair_t halves;
  logic [3:0] halves_seen;
  int halves_runs = 0;
  always_comb begin
    halves.low = ~low_level ^ 4'(poke);
    halves.low = low_level;
    halves.high = 4'h2;
  end
  always_comb begin
    halves_seen = halves.high;
    halves_runs = halves_runs + 1;
  end

  string text_level = "a";
  string text;
  string text_seen;
  int text_runs = 0;
  always_comb begin
    text = $sformatf("glitch %0d", poke);
    text = text_level;
    text.putc(0, "z");
    text = text_level;
  end
  always_comb begin
    text_seen = text;
    text_runs = text_runs + 1;
  end

  typedef struct {
    int count;
    string name;
  } named_t;
  int named_level = 1;
  named_t named;
  int named_seen;
  int named_runs = 0;
  always_comb begin
    named.count = poke + 50;
    named.name = "glitch";
    named.count = named_level;
    named.name = "";
  end
  always_comb begin
    named_seen = named.count + named.name.len();
    named_runs = named_runs + 1;
  end

  int line_level[$] = '{1, 2, 3};
  int line[$];
  int line_seen;
  int line_runs = 0;
  always_comb begin
    line = line_level;
    line.push_back(poke);
    line.push_front(0);
    void'(line.pop_back());
    void'(line.pop_front());
  end
  always_comb begin
    line_seen = line.size() > 1 ? line[1] : -1;
    line_runs = line_runs + 1;
  end

  int dynamic_level = 5;
  int dynamic[];
  int dynamic_seen;
  int dynamic_runs = 0;
  always_comb begin
    dynamic = new[4];
    dynamic[1] = poke + 9;
    dynamic[1] = dynamic_level;
  end
  always_comb begin
    dynamic_seen = dynamic[1];
    dynamic_runs = dynamic_runs + 1;
  end

  int entry_level = 1;
  int entries[string];
  int entries_seen;
  int entries_runs = 0;
  always_comb begin
    entries.delete();
    entries["kept"] = entry_level;
    entries[$sformatf("glitch %0d", poke)] = 1;
    entries.delete($sformatf("glitch %0d", poke));
  end
  always_comb begin
    entries_seen = entries.num() + entries["kept"];
    entries_runs = entries_runs + 1;
  end

  Token token_a = new(1);
  Token token_b = new(2);
  logic token_choice = 1'b0;
  Token held;
  int held_seen;
  int held_runs = 0;
  always_comb begin
    held = poke[0] ? token_a : token_b;
    held = token_choice ? token_b : token_a;
  end
  always_comb begin
    held_seen = held == token_a ? 1 : 2;
    held_runs = held_runs + 1;
  end

  typedef union {
    logic [7:0] wide;
    logic [3:0] narrow;
  } member_t;
  logic [7:0] member_level = 8'h11;
  member_t member;
  logic [7:0] member_seen;
  int member_runs = 0;
  always_comb begin
    member.wide = member_level;
    member.narrow = 4'(~member_level) ^ 4'(poke);
    member.wide = member_level;
  end
  always_comb begin
    member_seen = member.wide;
    member_runs = member_runs + 1;
  end

  // A memory declared packed, as older Verilog had to, read by an element a
  // constant names or by one an index chooses (LRM 7.4.1, 11.5.3).
  logic [255:0][7:0] packed_level = '0;
  logic [255:0][7:0] packed_mem;
  logic [7:0] packed_index = 8'd3;
  logic [7:0] packed_fixed_seen;
  logic [7:0] packed_any_seen;
  int packed_fixed_runs = 0;
  int packed_any_runs = 0;
  always_comb begin
    packed_mem = packed_level;
    packed_mem[3] = 8'h22 ^ 8'(poke);
    packed_mem[250] = 8'h33;
    packed_mem[3] = packed_level[3];
    packed_mem[250] = packed_level[250];
  end
  always_comb begin
    packed_fixed_seen = packed_mem[3];
    packed_fixed_runs = packed_fixed_runs + 1;
  end
  always_comb begin
    packed_any_seen = packed_mem[packed_index];
    packed_any_runs = packed_any_runs + 1;
  end

  logic [7:0] sliced_level = 8'h00;
  logic [7:0] sliced[4];
  logic sliced_seen;
  int sliced_runs = 0;
  always_comb begin
    sliced = '{default: '0};
    sliced[0:1] = '{8'h05, 8'h05 ^ 8'(poke)};
    sliced[0:1] = '{8'h00, sliced_level};
  end
  always_comb begin
    sliced_seen = sliced[1][0];
    sliced_runs = sliced_runs + 1;
  end

  // A register file's next state: the current one, with one entry replaced.
  int regs[4] = '{10, 11, 12, 13};
  int write_at = 0;
  int write_data = 10;
  int regs_next[4];
  int regs_seen;
  int regs_runs = 0;
  always_comb begin
    regs_next = regs;
    regs_next[write_at] = write_data;
  end
  always_comb begin
    regs_seen = regs_next[2];
    regs_runs = regs_runs + 1;
  end

  // A write past the last element lands where no read reaches (LRM 7.4.5),
  // which is no element and changes nothing.
  int bounded_level = 6;
  int outside = 9;
  int bounded[4];
  int bounded_seen;
  int bounded_runs = 0;
  always_comb begin
    bounded = '{default: 0};
    bounded[outside] = poke;
    bounded[1] = bounded_level;
  end
  always_comb begin
    bounded_seen = bounded[1];
    bounded_runs = bounded_runs + 1;
  end

  // A part nested deeper than a path follows is named only as the part the
  // path reaches, which holds its neighbours too, so what it held is not kept
  // and each write of it triggers as the standard reads.
  int deep_level = 0;
  int deep[2][2][2][2][2][2][2];
  int deep_seen;
  int deep_runs = 0;
  always_comb begin
    deep[0][0][0][0][0][0][0] = poke + 1;
    deep[0][0][0][0][0][0][0] = deep_level;
    deep[0][0][0][0][0][0][1] = 7;
  end
  always_comb begin
    deep_seen = deep[0][0][0][0][0][0][1];
    deep_runs = deep_runs + 1;
  end

  // A reference into an array whose size changes names no position in it,
  // since it goes on naming its element as the array is resized (LRM 13.5.2):
  // a write through it is of the array, which an execution keeps whole where it
  // wrote the array first, and is a change of its own where it did not.
  int lent_line_level = 3;
  int lent_line[];
  int lent_line_seen;
  int lent_line_runs = 0;
  function automatic void put_int(ref int target, input int value,
                                  input int unused);
    target = value + 1;
    target = value;
  endfunction
  always_comb begin
    lent_line = new[2];
    put_int(lent_line[1], lent_line_level, poke);
  end
  always_comb begin
    lent_line_seen = lent_line[1];
    lent_line_runs = lent_line_runs + 1;
  end

  int lent_only[] = '{0, 3};
  int lent_only_seen;
  int lent_only_runs = 0;
  always_comb put_int(lent_only[1], 3, poke);
  always_comb begin
    lent_only_seen = lent_only[1];
    lent_only_runs = lent_only_runs + 1;
  end

  // What an execution writes of what the procedure reads is no event for it,
  // since it was running and not waiting (LRM 9.4.2): an element written and
  // the array read by an index, the whole of which is what it reads.
  int own_in = 0;
  int own[2];
  int own_at = 1;
  int own_seen;
  int own_runs = 0;
  always_comb begin
    own[0] = own_in;
    own_seen = own[own_at];
    own_runs = own_runs + 1;
  end

  // Written by no combinational procedure, each write is a change (LRM 4.3).
  logic stepped = 1'b0;
  logic stepped_seen;
  int stepped_runs = 0;
  always_comb begin
    stepped_seen = stepped;
    stepped_runs = stepped_runs + 1;
  end

  // An execution writes back what it wrote through code of the design a library
  // call runs: `randomize()` calls the object's `post_randomize()` (LRM
  // 18.6.2).
  int hooked_in = 0;
  int hooked_out = 0;
  int hooked_seen;
  int hooked_runs = 0;
  class Hooked;
    rand bit [3:0] v;
    function void post_randomize();
      hooked_out = 0;
    endfunction
  endclass
  Hooked hooked = new;
  always_comb begin
    hooked_out = hooked_in;
    void'(hooked.randomize());
  end
  always_comb begin
    hooked_seen = hooked_out;
    hooked_runs = hooked_runs + 1;
  end

  // Nor is a general `always` or an `always_ff`, whatever it waits on.
  logic swing = 1'b0;
  logic swung = 1'b0;
  logic swung_seen;
  int swung_runs = 0;
  always @(swing) begin
    swung = 1'b1;
    swung = 1'b0;
  end
  always_comb begin
    swung_seen = swung;
    swung_runs = swung_runs + 1;
  end
  logic clk = 1'b0;
  logic flopped = 1'b0;
  logic flopped_seen;
  int flopped_runs = 0;
  always_ff @(posedge clk) begin
    flopped = 1'b1;
    flopped = 1'b0;
  end
  always_comb begin
    flopped_seen = flopped;
    flopped_runs = flopped_runs + 1;
  end

  // An execution that stops the procedure reading what it wrote tells it what
  // it wrote first, as every legal schedule has told it by then, so the
  // procedure is stopped ready to run and runs once started again (LRM 9.7).
  // One stopped while the execution writes what it reads, and started again by
  // the run, is told nothing of that write, which it was not waiting for, and
  // waits afresh.
  process halted_handle;
  logic halted_in = 1'b0;
  logic halted_seen;
  int halted_runs = 0;
  always_comb begin
    if (halted_handle == null) halted_handle = process::self();
    halted_seen = halted_in;
    halted_runs = halted_runs + 1;
  end
  logic halt = 1'b0;
  always_comb begin
    halted_in = halt;
    if (halt) halted_handle.suspend();
  end
  process rearmed_handle;
  logic rearmed_in = 1'b0;
  logic rearmed_seen;
  int rearmed_runs = 0;
  always_comb begin
    if (rearmed_handle == null) rearmed_handle = process::self();
    rearmed_seen = rearmed_in;
    rearmed_runs = rearmed_runs + 1;
  end
  logic rearm = 1'b0;
  always_comb begin
    rearmed_in = rearm;
    if (rearm) rearmed_handle.resume();
  end

  logic shared = 1'b0;
  logic shared_seen;
  int shared_runs = 0;
  always_comb begin
    shared_seen = shared;
    shared_runs = shared_runs + 1;
  end
  event share;
  initial @share shared = 1'b1;
  initial @share shared = 1'b0;

  logic updated = 1'b0;
  logic updated_seen;
  int updated_runs = 0;
  always_comb begin
    updated_seen = updated;
    updated_runs = updated_runs + 1;
  end

  logic source = 1'b0;
  logic delayed;
  assign #1 delayed = source;
  logic delayed_seen;
  int delayed_runs = 0;
  always_comb begin
    delayed_seen = delayed;
    delayed_runs = delayed_runs + 1;
  end

  // A force that an execution makes after writing what a procedure reads
  // through a port moves the procedure's wait to the forced storage (LRM
  // 10.6.2), so the write is told first, while the wait still watches what it
  // wrote, and the procedure runs for it. The force gives the port the value
  // the write left, so the force itself changes nothing the procedure reads.
  logic forced_source = 1'b0;
  logic forced_other = 1'b1;
  logic force_it = 1'b0;
  ForcedReader forced_reader (.in(forced_source));
  always_comb begin
    forced_source = force_it;
    if (force_it) force forced_reader.in = forced_other;
  end

  // An execution leaving a block of its own by disabling it changes how no
  // other process waits (LRM 9.6.2), so what it wrote before is still judged by
  // what the execution leaves.
  logic broke_out = 1'b1;
  logic break_it = 1'b0;
  logic broke_out_seen;
  int broke_out_runs = 0;
  always_comb begin
    broke_out = 1'b0;
    begin : leave
      if (break_it) disable leave;
    end
    broke_out = 1'b1;
  end
  always_comb begin
    broke_out_seen = broke_out;
    broke_out_runs = broke_out_runs + 1;
  end

  // An execution that resumes another such procedure that is not stopped, or
  // kills one, changes how no procedure that coalesces waits: resuming changes
  // nothing (LRM 9.7), and a killed process runs nothing whether it was told
  // first or not. So what the execution wrote is still judged by what it
  // leaves.
  process idle_handle;
  process doomed_handle;
  logic idle_in = 1'b0;
  logic idle_seen;
  logic doomed_in = 1'b0;
  logic doomed_seen;
  always_comb begin
    if (idle_handle == null) idle_handle = process::self();
    idle_seen = idle_in;
  end
  always_comb begin
    if (doomed_handle == null) doomed_handle = process::self();
    doomed_seen = doomed_in;
  end
  logic controlled = 1'b1;
  logic control_it = 1'b0;
  logic controlled_seen;
  int controlled_runs = 0;
  always_comb begin
    controlled = 1'b0;
    if (control_it) begin
      idle_handle.resume();
      doomed_handle.kill();
    end
    controlled = 1'b1;
  end
  always_comb begin
    controlled_seen = controlled;
    controlled_runs = controlled_runs + 1;
  end

  // A covergroup sampled on an edge the execution makes samples in the middle
  // of the execution (LRM 19.3), and waiting for its next edge changes how no
  // other process waits, so what the execution wrote is still judged by what it
  // leaves.
  logic sampled_out = 1'b1;
  logic sample_it = 1'b0;
  logic sampled_out_seen;
  int sampled_out_runs = 0;
  always_comb begin
    sampled_out = 1'b0;
    sampled_out = sample_it | 1'b1;
  end
  covergroup EdgeSample @(negedge sampled_out);
    coverpoint sample_it;
  endgroup
  EdgeSample edge_sample = new;
  always_comb begin
    sampled_out_seen = sampled_out;
    sampled_out_runs = sampled_out_runs + 1;
  end

  // An execution that ends its own process never waits again, and what it left
  // changed is told when it ends all the same.
  logic ended_in = 1'b0;
  logic ended_out;
  logic ended_seen;
  int ended_runs = 0;
  always_comb begin
    ended_out = 1'b0;
    ended_out = ended_in;
    if (ended_in) process::self().kill();
  end
  always_comb begin
    ended_seen = ended_out;
    ended_runs = ended_runs + 1;
  end

  task automatic expect_runs(input string what, input int got,
                             input int want);
    if (got !== want)
      $fatal(1, "%s: the procedure ran %0d times, expected %0d", what, got,
             want);
  endtask

  // Runs every glitching procedure once more and checks that no reader ran.
  task automatic expect_settled(input string what);
    int was[$];
    was = '{bit_runs, latch_runs, element_runs, wide_runs, ref_runs,
               real_runs, cleared_runs, refilled_runs, pair_runs,
               halves_runs, text_runs, named_runs, line_runs, dynamic_runs,
               entries_runs, held_runs, member_runs, packed_fixed_runs,
               packed_any_runs, sliced_runs, bounded_runs, lent_line_runs};
    poke = poke + 1;
    #1;
    expect_runs({what, ": a bit"}, bit_runs - was[0], 0);
    expect_runs({what, ": a latch's output"}, latch_runs - was[1], 0);
    expect_runs({what, ": an element"}, element_runs - was[2], 0);
    expect_runs({what, ": both ends of a wide value"}, wide_runs - was[3],
                0);
    expect_runs({what, ": through a reference"}, ref_runs - was[4], 0);
    expect_runs({what, ": a real"}, real_runs - was[5], 0);
    expect_runs({what, ": an array cleared, its element written"},
                cleared_runs - was[6], 0);
    expect_runs({what, ": an element written, the array cleared"},
                refilled_runs - was[7], 0);
    expect_runs({what, ": a structure cleared, its members written"},
                pair_runs - was[8], 0);
    expect_runs({what, ": a member"}, halves_runs - was[9], 0);
    expect_runs({what, ": a string, changed where it lies"},
                text_runs - was[10], 0);
    expect_runs({what, ": a structure holding a string"},
                named_runs - was[11], 0);
    expect_runs({what, ": a queue grown and shrunk"}, line_runs - was[12],
                0);
    expect_runs({what, ": a dynamic array made and its element written"},
                dynamic_runs - was[13], 0);
    expect_runs({what, ": an entry made and deleted"},
                entries_runs - was[14], 0);
    expect_runs({what, ": a class handle"}, held_runs - was[15], 0);
    expect_runs({what, ": a union's member"}, member_runs - was[16], 0);
    expect_runs({what, ": a packed memory's element named"},
                packed_fixed_runs - was[17], 0);
    expect_runs({what, ": a packed memory's element indexed"},
                packed_any_runs - was[18], 0);
    expect_runs({what, ": a slice"}, sliced_runs - was[19], 0);
    expect_runs({what, ": a write past the last element"},
                bounded_runs - was[20], 0);
    expect_runs({what, ": an array made whole, its element by reference"},
                lent_line_runs - was[21], 0);
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
    if (said != "x" || heard != "y")
      $fatal(1, "the string procedures settled at %s %s", said, heard);
    if (lent_a[1] !== 1'b1 || lent_b[1] !== 1'b1)
      $fatal(1, "the procedures writing through references settled at %b %b",
             lent_a[1], lent_b[1]);
    if (overlay_a.wide !== 8'h01 || overlay_b.wide !== 8'h01)
      $fatal(1, "the procedures writing union members settled at %h %h",
             overlay_a.wide, overlay_b.wide);

    expect_settled("written and written back");
    expect_settled("written and written back again");

    // A level moved: the reader of what it reaches runs once.
    base = bit_runs;
    bit_level = 1'b1;
    #1 expect_runs("a bit an execution leaves changed", bit_runs - base, 1);
    if (bit_seen !== 1'b1)
      $fatal(1, "the procedure saw %b, expected 1", bit_seen);
    base = latch_runs;
    latch_level = 1'b1;
    #1 expect_runs("a latch's output an execution leaves changed",
                   latch_runs - base, 1);
    base = element_runs;
    element_level = 7;
    #1 expect_runs("an element an execution leaves changed",
                   element_runs - base, 1);
    base = wide_runs;
    wide_level[128] = 1'b1;
    #1 expect_runs("a wide value's middle an execution leaves changed",
                   wide_runs - base, 1);
    base = real_runs;
    real_level = 2.5;
    #1 expect_runs("a real an execution leaves changed", real_runs - base, 1);
    base = text_runs;
    text_level = "b";
    #1 expect_runs("a string an execution leaves changed", text_runs - base, 1);
    if (text_seen != "b") $fatal(1, "the procedure saw %s", text_seen);
    base = line_runs;
    line_level[1] = 9;
    #1 expect_runs("a queue an execution leaves changed", line_runs - base, 1);
    if (line_seen != 9)
      $fatal(1, "the procedure saw %0d of the queue", line_seen);
    base = dynamic_runs;
    dynamic_level = 6;
    #1 expect_runs("a dynamic array an execution leaves changed",
                   dynamic_runs - base, 1);
    base = entries_runs;
    entry_level = 4;
    #1 expect_runs("an entry an execution leaves changed",
                   entries_runs - base, 1);
    base = held_runs;
    token_choice = 1'b1;
    #1 expect_runs("a class handle an execution leaves changed",
                   held_runs - base, 1);
    if (held_seen != 2) $fatal(1, "the procedure saw token %0d", held_seen);
    base = member_runs;
    member_level = 8'h12;
    #1 expect_runs("a union an execution leaves changed",
                   member_runs - base, 1);
    base = sliced_runs;
    sliced_level = 8'h01;
    #1 expect_runs("a slice leaving the bit read changed", sliced_runs - base,
                   1);
    base = bounded_runs;
    bounded_level = 8;
    #1 expect_runs("an element written beside a write past the last",
                   bounded_runs - base, 1);
    if (bounded_seen != 8)
      $fatal(1, "the procedure saw %0d of the element", bounded_seen);

    // A part written whole is compared whole, so a change anywhere in it wakes
    // a reader of any of it, whose execution then changes nothing.
    base = pair_runs;
    pair_level = 4'h3;
    #1 expect_runs("a structure written whole, the member read changed",
                   pair_runs - base, 1);
    base = halves_runs;
    low_level = 4'h9;
    #1 expect_runs("a member the procedure does not read changed",
                   halves_runs - base, 0);
    base = cleared_runs;
    cleared_level = 8'h05;
    #1 expect_runs("an array cleared whole, the element read changed",
                   cleared_runs - base, 1);
    base = named_runs;
    named_level = 2;
    #1 expect_runs("a structure holding a string, its count changed",
                   named_runs - base, 1);

    // The bits a packed value's change moved are what is told, so a reader of
    // other bits is passed over.
    base = packed_fixed_runs;
    base2 = packed_any_runs;
    packed_level[200] = 8'h11;
    #1 expect_runs("a packed memory's element the constant does not name",
                   packed_fixed_runs - base, 0);
    expect_runs("a packed memory's element changed, read by an index",
                packed_any_runs - base2, 1);
    base = packed_fixed_runs;
    packed_level[3] = 8'h55;
    #1 expect_runs("a packed memory's element read changed",
                   packed_fixed_runs - base, 1);
    if (packed_fixed_seen !== 8'h55)
      $fatal(1, "the procedure saw %h of the packed element",
             packed_fixed_seen);

    base = deep_runs;
    poke = poke + 1;
    #1 expect_runs("a deep element beside the one read written back",
                   deep_runs - base, 1);

    expect_settled("written back once each level moved");

    // A register file written whole and then at one entry: the entry read
    // changes only where the write lands on it.
    base = regs_runs;
    write_at = 2;
    write_data = 12;
    #1 expect_runs("an entry written with what it held", regs_runs - base, 0);
    base = regs_runs;
    write_data = 99;
    #1 expect_runs("the entry read written with a new value", regs_runs - base,
                   1);
    if (regs_seen != 99)
      $fatal(1, "the procedure saw %0d of the entry", regs_seen);
    base = regs_runs;
    write_at = 1;
    #1 expect_runs("the write moved to another entry", regs_runs - base, 1);
    if (regs_seen != 12)
      $fatal(1, "the procedure saw %0d of the entry", regs_seen);

    base = lent_only_runs;
    poke = poke + 1;
    #1 expect_runs("an element of a dynamic array written back by reference",
                   lent_only_runs - base, 1);

    base = own_runs;
    own_in = 4;
    #1 expect_runs("an execution writing part of what it reads",
                   own_runs - base, 1);

    // Each write by an `initial` is a change for the procedure reading it.
    base = stepped_runs;
    stepped = 1'b1;
    stepped = 1'b0;
    #1 expect_runs("written and written back by an initial procedure",
                   stepped_runs - base, 1);

    base = hooked_runs;
    hooked_in = 5;
    #1 expect_runs("written back by a hook a library call runs",
                   hooked_runs - base, 0);

    base = swung_runs;
    swing = 1'b1;
    #1 expect_runs("written and written back by an always procedure",
                   swung_runs - base, 1);
    base = flopped_runs;
    clk = 1'b1;
    #1 expect_runs("written and written back by an always_ff procedure",
                   flopped_runs - base, 1);

    base = halted_runs;
    halt = 1'b1;
    #1 expect_runs("stopped by the execution that wrote what it reads",
                   halted_runs - base, 0);
    halted_handle.resume();
    #1 expect_runs("started again after that execution", halted_runs - base, 1);
    rearmed_handle.suspend();
    #1 base = rearmed_runs;
    rearm = 1'b1;
    #1 expect_runs("started again by the execution that wrote what it reads",
                   rearmed_runs - base, 0);
    if (rearmed_seen !== 1'b0)
      $fatal(1, "the procedure started again saw %b", rearmed_seen);

    base = shared_runs;
    ->share;
    #1 expect_runs("a change one execution made and another undid",
                   shared_runs - base, 1);

    base = updated_runs;
    updated <= 1'b1;
    updated <= 1'b0;
    #1 expect_runs("two nonblocking updates undoing each other",
                   updated_runs - base, 1);

    base = delayed_runs;
    source = 1'b1;
    #2 expect_runs("a delayed assignment maturing", delayed_runs - base, 1);
    if (delayed_seen !== 1'b1)
      $fatal(1, "the procedure saw %b of the delayed update", delayed_seen);

    base = forced_reader.runs;
    force_it = 1'b1;
    #1 expect_runs("written and then forced by the same execution",
                   forced_reader.runs - base, 1);
    release forced_reader.in;

    base = broke_out_runs;
    break_it = 1'b1;
    #1 expect_runs("written back by an execution that left a block of its own",
                   broke_out_runs - base, 0);
    base = controlled_runs;
    control_it = 1'b1;
    #1 expect_runs(
        "written back by an execution that resumed and killed others",
                   controlled_runs - base, 0);
    base = sampled_out_runs;
    sample_it = 1'b1;
    #1 expect_runs("written back by an execution sampled on its own edge",
                   sampled_out_runs - base, 0);

    base = ended_runs;
    ended_in = 1'b1;
    #1 expect_runs("written by an execution that ended its own process",
                   ended_runs - base, 1);
    if (ended_seen !== 1'b1)
      $fatal(1, "the procedure saw %b of the ended execution's output",
             ended_seen);

    $display("All checks passed");
  end
endmodule
