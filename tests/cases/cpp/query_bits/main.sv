module Top;
  typedef logic [3:0] mubi_t;
  typedef struct packed {
    logic a;
    logic [2:0] b;
  } sp_t;
  typedef enum logic [1:0] {E0, E1, E2} e_t;

  mubi_t q;
  bit [1:0][7:0] m2d;

  int bits_type_rt;
  int bits_value_rt;
  int bits_struct_rt;
  int bits_enum_rt;
  int bits_2d_rt;

  // `$bits` used as a part-select bound: the `$bits`-derived high bound is
  // `integer` (4-state), the literal low bound is `int` (2-state), so the
  // endpoint-ordering comparison must reconcile the two domains.
  logic [2:0] slice_rt;
  logic red_rt;
  logic ca_red;
  assign ca_red = ^q[$bits(mubi_t)-1:1];

  // LRM 20.6.2: a dynamically sized operand reports the bit count of what it
  // currently holds -- its element count times the bits one element occupies --
  // and 0 while it holds nothing. A string's element is a byte.
  int    dyn[];
  int    que[$];
  string str;
  int    map[int];
  bit [6:0] narrow[$];

  int bits_dyn;
  int bits_queue;
  int bits_string;
  int bits_assoc;
  int bits_narrow;
  int bits_empty;

  // LRM 20.6.2 over a value whose elements are themselves dynamically sized:
  // each element contributes its current bit count, so the total sums the
  // elements' widths rather than multiplying by one fixed element width.
  int    nest[][];
  string strq[$];
  int    fixnest[2][];

  int bits_nest;
  int bits_strq;
  int bits_fixnest;

  initial begin
    q = 4'b1010;
    bits_type_rt = $bits(mubi_t);
    bits_value_rt = $bits(q);
    bits_struct_rt = $bits(sp_t);
    bits_enum_rt = $bits(e_t);
    bits_2d_rt = $bits(m2d);
    slice_rt = q[$bits(mubi_t)-1:1];
    red_rt = ^q[$bits(mubi_t)-1:1];

    dyn = new[4];
    que.push_back(1);
    que.push_back(2);
    que.push_back(3);
    str = "hello";
    map[10] = 1;
    map[20] = 2;
    narrow.push_back(7'h5);

    bits_dyn = $bits(dyn);
    bits_queue = $bits(que);
    bits_string = $bits(str);
    bits_assoc = $bits(map);
    bits_narrow = $bits(narrow);

    que.delete();
    bits_empty = $bits(que);

    nest = new[2];
    nest[0] = new[3];
    nest[1] = new[5];
    strq.push_back("hi");
    strq.push_back("world");
    fixnest[0] = new[1];
    fixnest[1] = new[4];

    bits_nest = $bits(nest);
    bits_strq = $bits(strq);
    bits_fixnest = $bits(fixnest);
  end
endmodule
