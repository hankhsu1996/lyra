// Where an untagged union is used whole, the standard names its first-declared
// member as what is used, whichever member was last written: `%p` prints only
// that member (LRM 21.2.1.6), and a streaming concatenation streams it, both
// packing the union and unpacking into it (LRM 11.4.14.1). The two members here
// are structures sharing a common initial sequence, so what the first member
// reads after a write through the second is defined (LRM 7.3) -- its tag, and
// not its count, which lies outside that sequence.
module Top;
  typedef struct {
    int tag;
    int count;
  } first_t;

  typedef struct {
    int tag;
    shortint narrow;
  } second_t;

  typedef union {
    first_t first;
    second_t second;
  } u_t;

  // Whether `text` holds `part` anywhere.
  function automatic bit Contains(string text, string part);
    for (int i = 0; i + part.len() <= text.len(); i++)
      if (text.substr(i, i + part.len() - 1) == part) return 1'b1;
    return 1'b0;
  endfunction

  u_t written_second;
  u_t unpacked;

  string printed;
  bit [63:0] packed_bits;
  int unpacked_first_tag;
  int unpacked_first_count;
  int unpacked_second_tag;

  initial begin
    written_second.second.tag = 7;
    written_second.second.narrow = 3;
    printed = "unset";
    printed = $sformatf("%p", written_second);
    packed_bits = {>>{written_second}};

    {>>{unpacked}} = 64'h0000_0009_0000_0005;
    unpacked_first_tag = unpacked.first.tag;
    unpacked_first_count = unpacked.first.count;
    unpacked_second_tag = unpacked.second.tag;
  end

  final begin
    if (!Contains(printed, "'{first:'{tag:7, count:"))
      $fatal(1, "%%p printed %s, which is not the first member with tag 7",
             printed);
    if (packed_bits[63:32] !== 32'd7)
      $fatal(1, "packing streamed %h, expected the first member's tag 7 first",
             packed_bits);
    if (unpacked_first_tag !== 9)
      $fatal(1, "unpacking set the first member's tag to %0d, expected 9",
             unpacked_first_tag);
    if (unpacked_first_count !== 5)
      $fatal(1, "unpacking set the first member's count to %0d, expected 5",
             unpacked_first_count);
    if (unpacked_second_tag !== 9)
      $fatal(1, "the second member's tag read %0d after unpacking, expected 9",
             unpacked_second_tag);
    $display("All checks passed");
  end
endmodule
