// A parameterized class is a generic class, and only a specialization of it
// -- the generic class taken with one set of parameter values -- is a type.
// Each specialization has its own object layout and its own set of static
// properties, while two specializations whose parameters all match are one
// type and so share them. The unadorned class name used as a type denotes
// the default specialization, whereas as the prefix of the class scope
// resolution operator an explicit specialization form is required. A
// parameterized class may extend a parameterized class, passing on its own
// type parameter (LRM 8.25, 8.25.1).
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

  int vec8_first;
  int vec8_second;
  int vec16_first;
  int vec8_count;
  int vec16_count;
  logic [7:0] vec8_data;
  logic [7:0] vec8_truncated;
  logic [15:0] vec16_data;
  int box_default_made;
  int box_explicit_made;
  int box_byte_made;
  int box_int_value;
  byte box_byte_value;
  byte derived_x;
  byte derived_y;

  initial begin
    Vec v8;
    Vec v8_wide_write;
    Vec #(16) v16;
    Box b_default;
    Box #(int) b_explicit;
    Box #(byte) b_byte;
    Derived #(byte) d;

    vec8_first = Vec #()::incr();
    vec8_second = Vec #()::incr();
    vec16_first = Vec #(16)::incr();
    vec8_count = Vec #()::count;
    vec16_count = Vec #(16)::count;

    v8 = new;
    v8.data = 8'hCD;
    vec8_data = v8.data;

    v8_wide_write = new;
    v8_wide_write.data = 16'hDEAD;
    vec8_truncated = v8_wide_write.data;

    v16 = new;
    v16.data = 16'hDEAD;
    vec16_data = v16.data;

    b_default = new;
    b_explicit = new;
    b_byte = new;
    box_default_made = Box #()::made;
    box_explicit_made = Box #(int)::made;
    box_byte_made = Box #(byte)::made;

    b_default.value = 42;
    b_byte.value = 8'sd7;
    box_int_value = b_default.value;
    box_byte_value = b_byte.value;

    d = new;
    d.x = 8'sd5;
    d.y = 8'sd6;
    derived_x = d.x;
    derived_y = d.y;
  end

  final begin
    if (vec8_first !== 1)
      $fatal(1, "vec8_first was %0d, expected 1", vec8_first);
    if (vec8_second !== 2)
      $fatal(1, "vec8_second was %0d, expected 2", vec8_second);
    if (vec16_first !== 1)
      $fatal(1, "vec16_first was %0d, expected 1", vec16_first);
    if (vec8_count !== 2)
      $fatal(1, "vec8_count was %0d, expected 2", vec8_count);
    if (vec16_count !== 1)
      $fatal(1, "vec16_count was %0d, expected 1", vec16_count);
    if (vec8_data !== 8'hCD)
      $fatal(1, "vec8_data was %0h, expected cd", vec8_data);
    if (vec8_truncated !== 8'hAD)
      $fatal(1, "vec8_truncated was %0h, expected ad", vec8_truncated);
    if (vec16_data !== 16'hDEAD)
      $fatal(1, "vec16_data was %0h, expected dead", vec16_data);
    if (box_default_made !== 2)
      $fatal(1, "box_default_made was %0d, expected 2", box_default_made);
    if (box_explicit_made !== 2)
      $fatal(1, "box_explicit_made was %0d, expected 2", box_explicit_made);
    if (box_byte_made !== 1)
      $fatal(1, "box_byte_made was %0d, expected 1", box_byte_made);
    if (box_int_value !== 42)
      $fatal(1, "box_int_value was %0d, expected 42", box_int_value);
    if (box_byte_value !== 7)
      $fatal(1, "box_byte_value was %0d, expected 7", box_byte_value);
    if (derived_x !== 5)
      $fatal(1, "derived_x was %0d, expected 5", derived_x);
    if (derived_y !== 6)
      $fatal(1, "derived_y was %0d, expected 6", derived_y);
    $display("All checks passed");
  end
endmodule
