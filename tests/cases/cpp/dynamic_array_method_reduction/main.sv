module Top;
  int arr [];
  int s_sum, s_product;
  bit [7:0] bits [];
  bit [7:0] s_and, s_or, s_xor;

  // Empty-array reductions all return element-shape zero. LRM 7.12.3 is
  // silent on the empty case; this matches slang's ArrayReductionMethod
  // behaviour and is the slang-compat anchor.
  bit [7:0] empty_bits [];
  int empty_int [];
  bit [7:0] empty_and, empty_or, empty_xor;
  int empty_sum, empty_product;

  // Single-element reduction returns the element verbatim for every kind.
  int single [];
  int single_sum, single_product;

  initial begin
    arr = new[4];
    arr[0] = 10;
    arr[1] = 20;
    arr[2] = 30;
    arr[3] = 40;
    s_sum = arr.sum();           // 10+20+30+40 = 100
    s_product = arr.product();   // 10*20*30*40 = 240000

    bits = new[3];
    bits[0] = 8'h7C;
    bits[1] = 8'h2B;
    bits[2] = 8'h18;
    s_and = bits.and();   // 0x7C & 0x2B & 0x18 = 0x08
    s_or  = bits.or();    // 0x7C | 0x2B | 0x18 = 0x7F
    s_xor = bits.xor();   // 0x7C ^ 0x2B ^ 0x18 = 0x4F

    empty_sum     = empty_int.sum();
    empty_product = empty_int.product();
    empty_and     = empty_bits.and();
    empty_or      = empty_bits.or();
    empty_xor     = empty_bits.xor();

    single = new[1];
    single[0] = 42;
    single_sum     = single.sum();
    single_product = single.product();
  end
endmodule
