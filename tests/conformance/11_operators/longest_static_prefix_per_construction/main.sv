// A select whose index is a constant expression is part of its longest static
// prefix (LRM 11.5.3), and a genvar and a parameter are both constants. So what
// such a select waits on is the bit it names in that one construction: an
// event control on `v[i]` in a loop block, or on `v[N]` in a module, wakes when
// that bit changes and on no other (LRM 9.4.2), and a continuous assignment
// reading it follows it (LRM 10.3.2). Each block and each instance watches its
// own bit, whether the select is a bit-select, an element of a multidimensional
// packed array, or an indexed part-select.
//
// The other side of that is the write: one into some bits of a vector changes
// those bits and no others, so a procedure reading other bits is not woken by
// it (LRM 9.2.2.2.1), whatever form the write takes -- a part-select, a
// compound assignment, an increment, a nonblocking assignment, a member of a
// packed structure -- and however its bits fall across the vector's storage.
module Leaf #(parameter int N = 0) (input bit [7:0] v, output int wakes);
  initial wakes = 0;
  always @(v[N]) wakes++;
endmodule

module Top;
  bit [7:0] v;
  bit [3:0][7:0] m;

  int bit_wakes [8];
  bit follows [8];
  bit element_follows [4];
  int part_wakes [4];

  for (genvar i = 0; i < 8; i++) begin : by_bit
    initial bit_wakes[i] = 0;
    always @(v[i]) bit_wakes[i]++;
    assign follows[i] = v[i];
  end

  for (genvar i = 0; i < 4; i++) begin : by_element
    assign element_follows[i] = m[i][3];
    initial part_wakes[i] = 0;
    always @(v[i * 2 +: 2]) part_wakes[i]++;
  end

  int leaf_wakes [3];
  Leaf #(.N(2)) at_two (.v(v), .wakes(leaf_wakes[0]));
  Leaf #(.N(5)) at_five (.v(v), .wakes(leaf_wakes[1]));
  Leaf #(.N(7)) at_seven (.v(v), .wakes(leaf_wakes[2]));

  typedef struct packed {
    logic [3:0] hi;
    logic [3:0] lo;
  } nibbles_t;

  logic [127:0] wide;
  nibbles_t nibbles;
  int wide_runs;
  int nibble_runs;
  logic [7:0] wide_seen;
  logic [3:0] nibble_seen;

  always_comb begin
    wide_runs = wide_runs + 1;
    wide_seen = wide[71:64];
  end

  always_comb begin
    nibble_runs = nibble_runs + 1;
    nibble_seen = nibbles.lo;
  end

  task automatic expect_runs(input string what, input int got, input int want);
    if (got !== want)
      $fatal(1, "%s: the procedure ran %0d times, expected %0d", what, got,
             want);
  endtask

  initial begin
    int base;
    #1 v[2] = 1;
    #1 v[5] = 1;
    #1 m[1][3] = 1;
    #1 m[2][4] = 1;

    wide = '0;
    nibbles = '0;
    #1 base = wide_runs;
    wide[127:120] = 8'hff;
    #1 expect_runs("bits above the ones read", wide_runs - base, 0);
    base = wide_runs;
    wide[63:56] = 8'hff;
    #1 expect_runs("bits just below, in the word before", wide_runs - base, 0);
    base = wide_runs;
    wide[68:61] = 8'h00;
    #1 expect_runs("bits straddling the words with an unread part moved",
                   wide_runs - base, 0);
    base = wide_runs;
    wide[68:61] = 8'h3f;
    #1 expect_runs("bits straddling the words", wide_runs - base, 1);
    base = wide_runs;
    wide[71:64] = wide[71:64];
    #1 expect_runs("the bits read written with their own value",
                   wide_runs - base, 0);
    base = wide_runs;
    wide[71:68] += 4'd1;
    #1 expect_runs("a compound assignment to bits read", wide_runs - base, 1);
    base = wide_runs;
    wide[70:66]++;
    #1 expect_runs("an increment of bits read", wide_runs - base, 1);
    base = wide_runs;
    wide[75:70] = 6'b1111_00;
    #1 expect_runs("bits overlapping from above with only unread ones moved",
                   wide_runs - base, 0);
    base = wide_runs;
    wide[75:70] = 6'b1111_01;
    #1 expect_runs("bits overlapping from above", wide_runs - base, 1);
    base = wide_runs;
    wide[64] <= 1'bx;
    #1 expect_runs("a nonblocking x into a bit read", wide_runs - base, 1);
    base = wide_runs;
    wide[72] <= 1'b1;
    #1 expect_runs("a nonblocking write just above", wide_runs - base, 0);
    base = nibble_runs;
    nibbles.hi = 4'h3;
    #1 expect_runs("the member not read", nibble_runs - base, 0);
    base = nibble_runs;
    nibbles.lo = 4'hz;
    #1 expect_runs("the member read", nibble_runs - base, 1);
    if (wide_seen !== 8'b0101_101x || nibble_seen !== 4'hz)
      $fatal(1, "the procedures saw %b and %h", wide_seen, nibble_seen);
  end

  final begin
    for (int k = 0; k < 8; k++) begin
      if (bit_wakes[k] !== ((k == 2 || k == 5) ? 1 : 0))
        $fatal(1, "the block watching bit %0d woke %0d times", k, bit_wakes[k]);
      if (follows[k] !== v[k])
        $fatal(1, "the block following bit %0d holds %0d", k, follows[k]);
    end
    for (int k = 0; k < 4; k++) begin
      if (element_follows[k] !== (k == 1))
        $fatal(1, "the block following element %0d holds %0d", k,
               element_follows[k]);
      if (part_wakes[k] !== ((k == 1 || k == 2) ? 1 : 0))
        $fatal(1, "the block watching bits %0d and %0d woke %0d times", k * 2,
               k * 2 + 1, part_wakes[k]);
    end
    if (leaf_wakes[0] !== 1 || leaf_wakes[1] !== 1 || leaf_wakes[2] !== 0)
      $fatal(1, "the instances watching bits 2, 5 and 7 woke %0d, %0d, %0d",
             leaf_wakes[0], leaf_wakes[1], leaf_wakes[2]);
    $display("All checks passed");
  end
endmodule
