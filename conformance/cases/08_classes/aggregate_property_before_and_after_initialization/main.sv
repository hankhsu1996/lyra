// A property of an unpacked structure, union or array type is initialized like
// any other, in declaration order, and its value before that is undefined (LRM
// 8.7): an initializer calling a method that reads a property declared after
// it reads that value, which is still a value of the property's type, members
// and elements of every type included. One with no initializer holds its
// type's default, which for an untagged union is its first member's (Table
// 7-1). Once initialized the property holds what its declaration gives, a
// whole write replaces it, and a write to one member or one element changes
// that part alone (LRM 7.2, 7.3, 7.4).
module Top;
  typedef struct {
    int n;
    string s;
  } pair_t;
  typedef struct {
    int k;
    string q[$];
    int d[];
    pair_t p;
  } deep_t;
  typedef union {
    int n;
    shortint h;
  } num_t;
  typedef union tagged {
    string Text;
    int Count;
  } item_t;

  class Box;
    string early_text = Described();
    int early_count = Counted();
    string early_row = RowText();
    int early_num = NumRead();
    pair_t later = '{5, "x"};
    deep_t deep = '{k: 3, q: '{"a", "b"}, d: '{4, 5, 6}, p: '{1, "y"}};
    pair_t row[3];
    num_t num = MadeNum();
    num_t unset_num;
    item_t item = tagged Text "t";

    function string Described();
      return $sformatf("%0d %s", later.n, later.s);
    endfunction
    function int Counted();
      return deep.q.size() + deep.d.size();
    endfunction
    function string RowText();
      return row[1].s;
    endfunction
    function int NumRead();
      return num.n;
    endfunction
    function num_t MadeNum();
      num_t made;
      made.n = 9;
      return made;
    endfunction
  endclass

  int later_n;
  string later_s;
  int deep_q;
  int deep_d;
  string deep_p_s;
  string row_s = "unset";
  int row_n = -1;
  int again_n;
  string again_s;
  int grown_q;
  string part_s;
  int part_n;
  string elem_s;
  int other_n = -1;
  int num_n;
  int unset_n = -1;
  int num_h;
  string item_text;
  int item_count;
  string item_text_again;

  initial begin
    Box b;
    b = new;
    later_n = b.later.n;
    later_s = b.later.s;
    deep_q = b.deep.q.size();
    deep_d = b.deep.d.size();
    deep_p_s = b.deep.p.s;
    row_s = b.row[2].s;
    row_n = b.row[2].n;
    num_n = b.num.n;
    unset_n = b.unset_num.n;
    item_text = b.item.Text;

    b.later = '{7, "z"};
    b.deep.q.push_back("c");
    b.deep.p.s = "w";
    b.row[1].s = "m";
    again_n = b.later.n;
    again_s = b.later.s;
    grown_q = b.deep.q.size();
    part_s = b.deep.p.s;
    part_n = b.deep.p.n;
    elem_s = b.row[1].s;
    other_n = b.row[0].n;
    b.num.h = 3;
    num_h = b.num.h;
    b.item = tagged Count 4;
    item_count = b.item.Count;
    b.item = tagged Text "u";
    item_text_again = b.item.Text;
  end

  final begin
    if (later_n !== 5) $fatal(1, "later_n was %0d, expected 5", later_n);
    if (later_s != "x") $fatal(1, "later_s was %s, expected x", later_s);
    if (deep_q !== 2) $fatal(1, "deep_q was %0d, expected 2", deep_q);
    if (deep_d !== 3) $fatal(1, "deep_d was %0d, expected 3", deep_d);
    if (deep_p_s != "y") $fatal(1, "deep_p_s was %s, expected y", deep_p_s);
    if (row_s != "") $fatal(1, "row_s was %s, expected empty", row_s);
    if (row_n !== 0) $fatal(1, "row_n was %0d, expected 0", row_n);
    if (again_n !== 7) $fatal(1, "again_n was %0d, expected 7", again_n);
    if (again_s != "z") $fatal(1, "again_s was %s, expected z", again_s);
    if (grown_q !== 3) $fatal(1, "grown_q was %0d, expected 3", grown_q);
    if (part_s != "w") $fatal(1, "part_s was %s, expected w", part_s);
    if (part_n !== 1) $fatal(1, "part_n was %0d, expected 1", part_n);
    if (elem_s != "m") $fatal(1, "elem_s was %s, expected m", elem_s);
    if (other_n !== 0) $fatal(1, "other_n was %0d, expected 0", other_n);
    if (num_n !== 9) $fatal(1, "num_n was %0d, expected 9", num_n);
    if (unset_n !== 0) $fatal(1, "unset_n was %0d, expected 0", unset_n);
    if (num_h !== 3) $fatal(1, "num_h was %0d, expected 3", num_h);
    if (item_text != "t")
      $fatal(1, "item_text was %s, expected t", item_text);
    if (item_count !== 4)
      $fatal(1, "item_count was %0d, expected 4", item_count);
    if (item_text_again != "u")
      $fatal(1, "item_text_again was %s, expected u", item_text_again);
    $display("All checks passed");
  end
endmodule
