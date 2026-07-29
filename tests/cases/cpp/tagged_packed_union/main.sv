module Top;
  // LRM 7.3.2: two members need one tag bit and the widest is 32, so the
  // union is 33 bits with the tag at the MSB.
  typedef union tagged packed {
    void Invalid;
    int  Valid;
  } vint_t;

  // Five members need three tag bits and the widest is 12, so the union is 15
  // bits and a narrower member leaves a gap between the tag and its own bits.
  typedef union tagged packed {
    bit [4:0]  A;
    bit [9:0]  B;
    bit [1:0]  C;
    bit [11:0] D;
    bit [2:0]  E;
  } five_t;

  typedef struct packed {
    bit [3:0] hi;
    bit [3:0] lo;
  } pair_t;

  // The representation applies recursively: the inner union is an ordinary
  // integral member of the outer one.
  typedef union tagged packed {
    bit [7:0] Byte;
    bit [3:0] Nib;
  } inner_t;

  typedef union tagged packed {
    pair_t  Pair;
    inner_t Inner;
  } outer_t;

  vint_t     v;
  vint_t     copy_a;
  vint_t     copy_b;
  five_t     f;
  outer_t    o;
  pair_t     ps;

  int        read_valid;
  int        after_write;
  bit [32:0] raw_valid;
  bit [32:0] raw_written;
  bit [32:0] raw_invalid;
  bit        copies_equal;
  bit        copies_differ;
  int        vint_width;
  int        five_width;
  int        outer_width;

  bit [14:0] raw_b;
  bit [14:0] raw_b_written;
  int        case_result;
  int        if_result;
  int        ternary_result;
  int        void_arm;
  int        nested_result;
  int        member_struct_result;
  int        packed_struct_result;

  initial begin
    vint_width  = $bits(vint_t);
    five_width  = $bits(five_t);
    outer_width = $bits(outer_t);

    v = tagged Valid 42;
    read_valid = v.Valid;
    raw_valid  = v;

    // LRM 11.9: a member write requires the current tag to agree. It updates
    // the member bits only -- the tag keeps naming the same member.
    v.Valid = 7;
    after_write = v.Valid;
    raw_written = v;

    v = tagged Invalid;
    raw_invalid = v;

    // Whole-value copy and equality ride on the single-vector projection.
    copy_a = tagged Valid 9;
    copy_b = copy_a;
    copies_equal = (copy_a == copy_b);
    copy_b = tagged Valid 10;
    copies_differ = (copy_a != copy_b);

    f = tagged B 10'h2AA;
    raw_b = f;

    case (f) matches
      tagged A .x : case_result = -1;
      tagged B .x : case_result = int'(x);
      tagged C .x : case_result = -3;
      default     : case_result = -9;
    endcase

    if (f matches tagged B .x) if_result = int'(x) + 1;
    else if_result = -1;

    ternary_result = f matches tagged B .x ? int'(x) + 2 : -1;

    // A member write through a narrow member leaves both the tag and the
    // undefined gap between them untouched.
    f.B = 10'h155;
    raw_b_written = f;

    // A tagged pattern with no value pattern matches on the tag alone.
    if (v matches tagged Invalid) void_arm = 5;
    else void_arm = -1;

    o = tagged Inner (tagged Nib 4'hC);
    if (o matches tagged Inner (tagged Nib .n)) nested_result = int'(n);
    else nested_result = -1;

    // A structure pattern destructuring a packed-struct member of the union.
    o = tagged Pair '{hi: 4'h3, lo: 4'h5};
    if (o matches tagged Pair '{.a, .b})
      member_struct_result = int'(a) * 100 + int'(b);
    else member_struct_result = -1;

    // LRM 12.6: the same structure pattern with a packed struct as the
    // subject, no union involved.
    ps = '{hi: 4'h2, lo: 4'h9};
    case (ps) matches
      '{.a, .b} : packed_struct_result = int'(a) * 100 + int'(b);
    endcase
  end
endmodule
