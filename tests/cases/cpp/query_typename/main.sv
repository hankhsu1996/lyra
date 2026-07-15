// LRM 20.6.1 `$typename`: the resolved type of the operand, as a string. The
// operand is never evaluated, so it may be a data type as readily as a value,
// and an expression operand reports its self-determined type. The string is
// canonical: a typedef resolves back to the type it names, an anonymous
// unpacked array's element name is a `$` placeholder, a user-defined name is
// qualified by its declaring scope, and an enum's members carry their values.
module Top;
  typedef bit node;
  typedef enum bit [1:0] {A, B, C = 3} e_t;
  typedef struct packed {
    bit a;
    bit [2:0] b;
  } sp_t;

  node   arr[2:0];
  int    dyn[];
  e_t    e;
  sp_t   sp;
  string s;
  int    i;

  string tn_arr, tn_dyn, tn_enum, tn_struct, tn_string, tn_type, tn_expr;

  initial begin
    tn_arr = $typename(arr);
    tn_dyn = $typename(dyn);
    tn_enum = $typename(e);
    tn_struct = $typename(sp);
    tn_string = $typename(s);
    tn_type = $typename(bit [3:0]);
    tn_expr = $typename(i + i);
  end
endmodule
