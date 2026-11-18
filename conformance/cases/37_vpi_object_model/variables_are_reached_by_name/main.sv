// `vpi_handle_by_name` reaches a variable by its full name, or by the name it
// has within a scope it is handed (LRM 38.21), and reaches a part of one by
// the name the source would write for it: a member of a structure by the
// variable's name and each member's after it, an element of an array and a
// bit of a vector by the indices after it (LRM 37.17 detail 28). Each answers
// the full name that reaches it, its name -- the last identifier with the
// indices after it -- and its decompiled name, the whole of the way from the
// variable. A variable a generate block, a named block or a package declares
// is named through them. A name whose index lies outside the dimension it
// selects in names nothing (LRM 38.19), and so does a member no structure
// declares.
package P;
  int counter = 7;
endpackage

module Top;
  import "DPI-C" context function int look();

  typedef struct {
    integer i1;
    logic [1:4] vec[5:8];
    struct {
      shortint j1;
      byte b1;
    } inner1;
  } str_t;

  bit [7:0] arr1[1:4][9:15];
  str_t str1;
  logic [7:0] word;

  for (genvar i = 0; i < 2; i++) begin : g
    int in_block = i;
  end

  int differences = -1;

  initial begin : main
    static int kept;
    kept = 3;
    #1 differences = look();
  end

  final begin
    if (differences !== 0)
      $fatal(1, "the lookup found %0d differences", differences);
    $display("All checks passed");
  end
endmodule
