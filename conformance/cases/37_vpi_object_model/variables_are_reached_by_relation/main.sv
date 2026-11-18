// A scope's variables are reached by its relations to them (LRM 37.17): all
// of them, its logic variables and its arrays. A variable's parts are reached
// by its own relations: an array's elements through every unpacked dimension
// (detail 5), a structure's members (LRM 37.26), a packed array's elements one
// dimension at a time (LRM 37.18 detail 3), a vector's bits (detail 12), and
// by index and by indices, the leftmost dimension first (LRM 38.19, 38.20).
// A part answers its indices from its own outward (details 5, 13), those of a
// part of a packed array going no further than the array, its parent (LRM
// 37.18 detail 6), the ranges of the dimensions it has (detail 4) and the
// bounds of the leftmost (detail 6), and has as its parent the nearest of the
// objects detail 26 names. A dynamic array and an associative array hold the
// elements they hold when they are asked, and a range of one sized by the run
// is what it holds then (LRM 37.22), an associative array's none;
// an index past a fixed range or past what a dynamic array holds reaches
// nothing. An associative array's elements come in the order of its indices,
// each named by its index as the index type reads it (LRM 7.9.4) -- an `int`,
// a `byte`, an unsigned 64-bit index past the largest signed one, a negative
// one wider than 64 bits, a string -- whatever the element is, a structure or
// a class handle among them, and an array indexed by a class holds its
// entries as any other does. An index foreign code hands is the index of the
// select it would write, converted into the index type (LRM 7.8.4, 38.19), so
// 300 names the entry of a `byte` index at 44; a wildcard index is read
// unsigned, so -1 and 4294967295 name one entry, and names its entry whatever
// width its key was written at (LRM 7.8.1). The full name of an entry names it
// again, a `string` key written as the string literal it is, where an escaped
// character no escape sequence names is that character (LRM 5.9). An index is
// the one the source writes, so index 6 is the second element of an array
// declared [5:8] and the third of one declared [8:5] (LRM 38.19); and a bit of
// a packed value is one object however it is named, so `ps[0]` and `ps.b[7]`
// below are one (LRM 38.3).
module Top;
  import "DPI-C" context function int look();

  typedef struct packed {
    bit [3:0] hi;
    bit [3:0] lo;
  } nibbles_t;
  typedef struct {
    int a;
    bit [1:0][2:3] bvec[4:5];
  } holder_t;

  logic [7:0] lv;
  int up[5:8];
  int down[8:5];
  struct packed {
    int a;
    logic [0:7] b;
  } ps;
  int ua2[2][3];
  nibbles_t [1:0] pa;
  nibbles_t [1:0][3:0] upa[2];
  holder_t spa[9:11][12:13];
  int da[];
  int aa[int];
  int by_byte[byte];
  class Key;
  endclass
  int by_object[Key];
  int by_wide[bit [63:0]];
  int wild[*];
  holder_t by_holder[int];
  Key handles[byte];
  int by_huge[logic signed [99:0]];
  string sa[string];
  int quoted[string];

  int differences = -1;

  initial begin
    static Key key = new;
    da = new[3];
    up[6] = 16;
    down[6] = 26;
    aa[9] = 1;
    aa[5] = 2;
    aa[-3] = 3;
    by_byte[44] = 4;
    by_object[key] = 5;
    by_wide[64'h8000_0000_0000_0001] = 6;
    wild[10'd300] = 8;
    wild[64'h1_0000_0000] = 9;
    by_holder[3].a = 11;
    handles[44] = key;
    by_huge[-100'sd100000000000000000000] = 12;
    sa["one"] = "1";
    quoted["say \"hi\"\\"] = 6;
    quoted[""] = 1;
    quoted["]"] = 2;
    #1 differences = look();
  end

  final begin
    if (differences !== 0)
      $fatal(1, "the relations differ in %0d answers", differences);
    $display("All checks passed");
  end
endmodule
