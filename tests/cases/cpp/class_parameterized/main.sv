// LRM 8.25 parameterized classes. Each specialization is its own type:
// distinct instance layout, distinct static-property cells, distinct
// construction. Bare `Vec v;` denotes the default specialization `Vec #()`;
// LRM 8.25.1 requires the scope-resolution operator to always carry an
// explicit parameter override (`Vec #()::count`, not `Vec::count`). Vec
// covers value parameters, Box covers type parameters (both facets of LRM
// 8.25), and Derived extending Base #(T) covers the parameterized-base
// forwarding rule (Derived #(byte) picks Base #(byte)).
module Top;

  class Vec #(int W = 8);
    logic [W-1:0] data;
    static int count = 0;
    static function int incr();
      count = count + 1;
      return count;
    endfunction
  endclass

  class Box #(type T = int);
    T value;
    static int made = 0;
    function new();
      made = made + 1;
    endfunction
  endclass

  class Base #(type T = int);
    T x;
  endclass

  class Derived #(type T = int) extends Base #(T);
    T y;
  endclass

  int vec8_incr1;
  int vec8_incr2;
  int vec16_incr1;
  int vec8_count_final;
  int vec16_count_final;

  logic [7:0]  vec8_data;
  logic [15:0] vec16_data;

  int box_int_made;
  int box_byte_made;
  int box_int_value;
  byte box_byte_value;

  byte derived_x;
  byte derived_y;

  initial begin
    Vec v8_a, v8_b;
    Vec #(16) v16;
    Box b_int;
    Box #(byte) b_byte;
    Derived #(byte) d;

    vec8_incr1 = Vec #()::incr();
    vec8_incr2 = Vec #()::incr();
    vec16_incr1 = Vec #(16)::incr();

    v8_a = new;   v8_a.data = 8'hAB;
    v8_b = new;   v8_b.data = 8'hCD;
    v16  = new;   v16.data  = 16'hDEAD;

    b_int  = new;   b_int.value  = 42;
    b_byte = new;   b_byte.value = 8'sd7;

    d = new;   d.x = 8'sd5;   d.y = 8'sd6;

    vec8_count_final  = Vec #()::count;
    vec16_count_final = Vec #(16)::count;

    vec8_data  = v8_b.data;
    vec16_data = v16.data;

    box_int_made   = Box #()::made;
    box_byte_made  = Box #(byte)::made;
    box_int_value  = b_int.value;
    box_byte_value = b_byte.value;

    derived_x = d.x;
    derived_y = d.y;
  end
endmodule
