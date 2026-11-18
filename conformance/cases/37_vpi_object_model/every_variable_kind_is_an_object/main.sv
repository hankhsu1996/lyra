// Every kind of variable LRM 37.17 lists is an object of its own type, and
// answers what it is: its size (detail 9), whether it is signed, a scalar or a
// vector (detail 20), an array's kind (detail 21), and whether a structure or a
// union is packed, tagged or soft (LRM 37.26). Its typespec is the type as the
// source declared it (LRM 37.25): a typedef's name, the typedef it aliases, an
// array's element and an associative array's index typespec one dimension at a
// time, an enumeration's base typespec and constants, and a structure's members
// with their own typespecs; a class's is the class, whatever typedef names it,
// and is named (LRM 37.32 details 1, 4). An enumeration is signed as its base
// is. The size of a string or an array whose size the run sets is what it holds
// when it is asked.
module Top;
  import "DPI-C" context function int look();

  typedef enum bit [1:0] {kRed, kGreen, kBlue} colour_t;
  typedef colour_t paint_t;
  typedef enum {kBelow = -1, kAbove} side_t;
  typedef int count_t;
  typedef struct packed {
    bit [3:0] hi;
    bit [3:0] lo;
  } nibbles_t;
  typedef union packed {
    bit [7:0] whole;
    nibbles_t parts;
  } byte_view_t;
  typedef union tagged {
    int number;
    real fraction;
  } either_t;
  typedef union soft {
    bit [7:0] wide;
    bit [3:0] narrow;
  } loose_t;
  typedef struct {
    int a;
    real b;
  } pair_t;
  class Item;
  endclass
  typedef Item item_t;

  logic sl;
  logic [7:0] lv;
  bit sb;
  bit [3:0] bv;
  logic [1:0][3:0] mda;
  int i;
  integer ig;
  byte by;
  shortint si;
  longint li;
  time t;
  real r;
  shortreal sr;
  realtime rt;
  paint_t e;
  side_t se;
  count_t c;
  nibbles_t ps;
  byte_view_t pu;
  loose_t su;
  nibbles_t [1:0] pa;
  either_t tu;
  pair_t us;
  int ua[0:3];
  int ua2[2][3];
  string s;
  chandle ch;
  Item cv;
  item_t named_cv;
  int da[];
  int q[$];
  int aa[int];
  int sa[string];

  int differences = -1;

  initial begin
    s = "abc";
    da = new[3];
    q = '{1, 2};
    aa[4] = 1;
    sa["x"] = 1;
    sa["y"] = 2;
    #1 differences = look();
  end

  final begin
    if (differences !== 0)
      $fatal(1, "the objects differ in %0d answers", differences);
    $display("All checks passed");
  end
endmodule
