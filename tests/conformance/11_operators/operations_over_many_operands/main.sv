// An operation the source writes over a list of operands takes any number of
// them: a concatenation of bits, of strings and of queue elements and queues
// (LRM 11.4.12, 11.4.12.2, 10.10), a replication and a stream of such a list
// (LRM 11.4.12.1, 11.4.14), and a set membership test over as many members
// (LRM 11.4.13), which is the OR of every comparison -- 1 where one matches, x
// where none does and one is ambiguous.
// A structure of as many members is equal to another where every member is,
// unequal where one is not, and x where none is unequal and one is unknown
// (LRM 11.4.5), and identical only where every member is bit for bit. A
// condition of as many clauses joined by `&&&` holds where every clause does,
// and not where one is false or unknown (LRM 12.6.2).
// `L256(a)` lists a[0] through a[255], in order, and `S256(a)` joins them by
// `&&&`.
`define L4(a, p) a[(p) * 4], a[(p) * 4 + 1], a[(p) * 4 + 2], a[(p) * 4 + 3]
`define L16(a, p) `L4(a, (p) * 4), `L4(a, (p) * 4 + 1), \
  `L4(a, (p) * 4 + 2), `L4(a, (p) * 4 + 3)
`define L64(a, p) `L16(a, (p) * 4), `L16(a, (p) * 4 + 1), \
  `L16(a, (p) * 4 + 2), `L16(a, (p) * 4 + 3)
`define L256(a) `L64(a, 0), `L64(a, 1), `L64(a, 2), `L64(a, 3)
`define S4(a, p) a[(p) * 4] &&& a[(p) * 4 + 1] &&& a[(p) * 4 + 2] &&& \
  a[(p) * 4 + 3]
`define S16(a, p) `S4(a, (p) * 4) &&& `S4(a, (p) * 4 + 1) &&& \
  `S4(a, (p) * 4 + 2) &&& `S4(a, (p) * 4 + 3)
`define S64(a, p) `S16(a, (p) * 4) &&& `S16(a, (p) * 4 + 1) &&& \
  `S16(a, (p) * 4 + 2) &&& `S16(a, (p) * 4 + 3)
`define S256(a) `S64(a, 0) &&& `S64(a, 1) &&& `S64(a, 2) &&& `S64(a, 3)
`define MEMBER(n) logic [3:0] n;
`define M4(p) `MEMBER(p``0) `MEMBER(p``1) `MEMBER(p``2) `MEMBER(p``3)
`define M16(p) `M4(p``0) `M4(p``1) `M4(p``2) `M4(p``3)
`define M64(p) `M16(p``0) `M16(p``1) `M16(p``2) `M16(p``3)
`define M256(p) `M64(p``0) `M64(p``1) `M64(p``2) `M64(p``3)

module Top;
  typedef struct {
    `M256(m)
  } wide_t;

  bit bits[256];
  byte words[256];
  string texts[256];
  int ones[256];

  bit [255:0] joined_bits;
  bit [2047:0] joined_words;
  bit [511:0] replicated;
  bit [255:0] streamed;
  string joined_text;
  int joined_queue[$];
  logic matched;
  logic unmatched;
  logic ambiguous;
  logic same;
  logic differs;
  logic unknown;
  bit identical;
  bit not_identical;
  bit clauses[256];
  logic open_clauses[256];
  bit all_held;
  bit one_false;
  bit one_unknown;

  initial begin
    int front[$];
    int back[$];
    wide_t a;
    wide_t b;

    foreach (bits[k]) bits[k] = k % 3 == 0;
    foreach (words[k]) words[k] = byte'(k);
    foreach (texts[k]) texts[k] = "t";
    foreach (ones[k]) ones[k] = 1;
    front = '{-1, -2};
    back = '{-3};

    joined_bits = '0;
    joined_words = '0;
    replicated = '0;
    streamed = '0;
    joined_text = "";
    matched = 1'b0;
    unmatched = 1'b1;
    ambiguous = 1'b0;
    same = 1'b0;
    differs = 1'b1;
    unknown = 1'b0;
    identical = 1'b0;
    not_identical = 1'b1;

    joined_bits = {`L256(bits)};
    joined_words = {`L256(words)};
    replicated = {2{`L256(bits)}};
    streamed = {<<{`L256(bits)}};
    joined_text = {`L256(texts)};
    joined_queue = {front, `L256(ones), back};

    matched = 8'd200 inside {`L256(words)};
    unmatched = 9'd300 inside {`L256(words)};
    ambiguous = 4'b1x00 inside {4'd3, 4'b1100, 4'd5};

    a = '{default: 4'd0};
    a.m0000 = 4'hF;
    b = a;
    same = a == b;
    b.m3333 = 4'd9;
    differs = a == b;
    b = a;
    b.m2000 = 4'b10x1;
    a.m2000 = 4'b1011;
    unknown = a == b;
    identical = b === b;
    not_identical = a === b;

    foreach (clauses[k]) clauses[k] = 1'b1;
    foreach (open_clauses[k]) open_clauses[k] = 1'b1;
    all_held = 1'b0;
    one_false = 1'b1;
    one_unknown = 1'b1;
    if (`S256(clauses)) all_held = 1'b1;
    clauses[255] = 1'b0;
    if (`S256(clauses)) one_false = 1'b1;
    else one_false = 1'b0;
    open_clauses[128] = 1'bx;
    if (`S256(open_clauses)) one_unknown = 1'b1;
    else one_unknown = 1'b0;
  end

  final begin
    if (joined_bits !== {{85{3'b100}}, 1'b1})
      $fatal(1, "a join of 256 bits held %h", joined_bits);
    if (joined_words[2047:2032] !== 16'h0001 || joined_words[15:0] !== 16'hFEFF)
      $fatal(1, "a join of 256 bytes held %h at its ends",
             {joined_words[2047:2032], joined_words[15:0]});
    if (replicated !== {2{joined_bits}})
      $fatal(1, "a replication of a 256-operand join held %h", replicated);
    if (streamed !== {1'b1, {85{3'b001}}})
      $fatal(1, "a stream of 256 operands held %h", streamed);
    if (joined_text.len() !== 256 || joined_text[255] !== "t")
      $fatal(1, "a join of 256 strings was %0d characters long",
             joined_text.len());
    if (joined_queue.size() !== 259 || joined_queue[0] !== -1 ||
        joined_queue[2] !== 1 || joined_queue[258] !== -3)
      $fatal(1, "a queue joined from 259 elements held %0d of them",
             joined_queue.size());
    if (matched !== 1'b1)
      $fatal(1, "a member of 256 values was found %b", matched);
    if (unmatched !== 1'b0)
      $fatal(1, "a value in none of 256 members was found %b", unmatched);
    if (ambiguous !== 1'bx)
      $fatal(1, "a value matching none and one ambiguously was found %b",
             ambiguous);
    if (same !== 1'b1)
      $fatal(1, "two equal structures of 256 members compared %b", same);
    if (differs !== 1'b0)
      $fatal(1, "structures differing in one member compared %b", differs);
    if (unknown !== 1'bx)
      $fatal(1, "structures with one ambiguous member compared %b", unknown);
    if (identical !== 1'b1)
      $fatal(1, "a structure holding x was not identical to itself");
    if (not_identical !== 1'b0)
      $fatal(1, "structures differing in an x bit were identical");
    if (all_held !== 1'b1)
      $fatal(1, "256 true clauses joined by &&& did not hold");
    if (one_false !== 1'b0)
      $fatal(1, "256 clauses joined by &&&, the last false, held");
    if (one_unknown !== 1'b0)
      $fatal(1, "256 clauses joined by &&&, one unknown, held");
    $display("All checks passed");
  end
endmodule
