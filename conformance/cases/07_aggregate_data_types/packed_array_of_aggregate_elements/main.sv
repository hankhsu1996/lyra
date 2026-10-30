// A packed array may be made of enumerated types and, recursively, of other
// packed arrays and packed structures, and it remains a single vector however
// its element type is composed. An element select therefore yields a value of
// the element type, so a member reached through an element select and a
// chained select into a nested packed array both address the bits the element
// occupies (LRM 7.4.1, 7.4.4, 7.2.1, 7.3.1).
module Top;
  typedef struct packed {
    logic [3:0] a;
    logic [3:0] b;
  } nibbles_t;

  typedef enum logic [1:0] {RED, GREEN, BLUE} color_t;

  typedef union packed {
    logic [7:0] full;
    logic [1:0][3:0] halves;
  } view_t;

  logic [15:0] struct_array_whole;
  logic [3:0] struct_array_field;
  logic [7:0] struct_array_element;
  logic [7:0] enum_array_whole;
  logic [1:0] enum_array_element;
  logic [15:0] union_array_whole;
  logic [7:0] union_array_member;
  logic [31:0] nested_whole;
  logic [7:0] nested_element;
  logic [3:0] nested_field;

  initial begin
    nibbles_t [1:0] struct_array;
    color_t [3:0] enum_array;
    view_t [1:0] union_array;
    nibbles_t [1:0][1:0] nested;

    struct_array[0].a = 4'h1;
    struct_array[0].b = 4'h2;
    struct_array[1] = 8'hAB;
    struct_array_whole = struct_array;
    struct_array_field = struct_array[0].a;
    struct_array_element = struct_array[1];

    enum_array[0] = GREEN;
    enum_array[1] = BLUE;
    enum_array[2] = RED;
    enum_array[3] = GREEN;
    enum_array_whole = enum_array;
    enum_array_element = enum_array[0];

    union_array[0] = 8'hCD;
    union_array[1] = 8'hAB;
    union_array_whole = union_array;
    union_array_member = union_array[0].full;

    nested[0][0] = 8'h11;
    nested[0][1] = 8'h22;
    nested[1][0] = 8'h33;
    nested[1][1] = 8'h44;
    nested_whole = nested;
    nested_element = nested[1][0];
    nested_field = nested[1][1].a;
  end

  final begin
    if (struct_array_whole !== 16'hAB12)
      $fatal(1, "struct_array_whole was %h, expected ab12",
             struct_array_whole);
    if (struct_array_field !== 4'h1)
      $fatal(1, "struct_array_field was %h, expected 1", struct_array_field);
    if (struct_array_element !== 8'hAB)
      $fatal(1, "struct_array_element was %h, expected ab",
             struct_array_element);
    if (enum_array_whole !== 8'h49)
      $fatal(1, "enum_array_whole was %h, expected 49", enum_array_whole);
    if (enum_array_element !== 2'h1)
      $fatal(1, "enum_array_element was %h, expected 1", enum_array_element);
    if (union_array_whole !== 16'hABCD)
      $fatal(1, "union_array_whole was %h, expected abcd", union_array_whole);
    if (union_array_member !== 8'hCD)
      $fatal(1, "union_array_member was %h, expected cd", union_array_member);
    if (nested_whole !== 32'h44332211)
      $fatal(1, "nested_whole was %h, expected 44332211", nested_whole);
    if (nested_element !== 8'h33)
      $fatal(1, "nested_element was %h, expected 33", nested_element);
    if (nested_field !== 4'h4)
      $fatal(1, "nested_field was %h, expected 4", nested_field);
    $display("All checks passed");
  end
endmodule
