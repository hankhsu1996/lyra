// First working SystemVerilog class (LRM 8): fields, `new`, member access
// (read and write), handle copy, and handle equality against null and against
// another handle. Field values and comparison results are copied into module
// variables so the test asserts the feature's product, not a print side effect.
module Top;
  class C;
    int x;
    int y;
  endclass

  int x_val;
  int y_val;
  bit eq_null;
  bit eq_copy;
  bit eq_distinct;

  initial begin
    C h;
    C g;
    h = new;
    h.x = 5;
    h.y = h.x + 7;
    x_val = h.x;
    y_val = h.y;

    g = h;
    eq_null = (h == null);
    eq_copy = (h == g);

    g = new;
    eq_distinct = (h == g);
  end
endmodule
