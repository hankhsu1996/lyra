// A tagged union held as a member of an untagged union is read as the bits
// another member wrote may leave it (LRM 7.3.2), and the standard does not say
// which value that read answers beyond its being one of the member's type. Lyra
// lays a tagged union out as a packed one is, its held member from the first
// bit and its tag above the widest member, so the bits another member writes
// may leave a tag naming no member, by its value or by an unknown bit. Lyra
// answers that with the first member at its default, as an uninitialized
// tagged union holds; a tag naming a member answers that member, made from the
// bits below the tag.
module Top;
  typedef union tagged {
    int a;
    logic [3:0] b;
    bit [1:0] c;
  } choice_t;

  typedef union {
    choice_t choice;
    logic [39:0] raw;
  } holder_t;

  holder_t past_the_members;
  holder_t unknown_tag;
  holder_t named;
  int read_past = -1;
  int read_unknown = -1;
  logic [3:0] read_named = 4'h0;

  initial begin
    // Tag 3 of three members names none.
    past_the_members.raw = 40'h03_0000_0009;
    if (past_the_members.choice matches tagged a .held) read_past = held;
    unknown_tag.raw = 40'h0x_0000_0009;
    if (unknown_tag.choice matches tagged a .held) read_unknown = held;
    // Tag 1 names `b`, made from the lowest four bits.
    named.raw = 40'h01_0000_0006;
    if (named.choice matches tagged b .held) read_named = held;
  end

  final begin
    if (read_past !== 0)
      $fatal(1, "a tag naming no member read %0d, expected 0", read_past);
    if (read_unknown !== 0)
      $fatal(1, "an unknown tag read %0d, expected 0", read_unknown);
    if (read_named !== 4'h6)
      $fatal(1, "a tag naming b read %h, expected 6", read_named);
    $display("All checks passed");
  end
endmodule
