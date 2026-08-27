// A conversion places its result in the variable the corresponding argument
// specifies, and that argument is a variable in whatever form the language
// writes one -- an element of a fixed or a dynamic array, a part-select of a
// packed value, or a member of a structure (LRM 21.3.4.3).
module Top;
  typedef struct packed {
    int a;
    int b;
  } pair;

  int elements[3];
  int dynamic_elements[];
  bit [15:0] packed_value;
  pair members;

  int into_elements;
  int into_dynamic;
  int into_part_select;
  int into_members;

  initial begin
    dynamic_elements = new[2];
    packed_value = 16'h0000;

    into_elements = $sscanf("10 20", "%d %d", elements[0], elements[1]);
    into_dynamic = $sscanf("30 40", "%d %d", dynamic_elements[0],
                           dynamic_elements[1]);
    into_part_select = $sscanf("ab", "%h", packed_value[7:0]);
    into_members = $sscanf("99 77", "%d %d", members.a, members.b);
  end

  final begin
    if (into_elements !== 2)
      $fatal(1, "writing two array elements returned %0d, expected 2",
             into_elements);
    if (elements[0] !== 10 || elements[1] !== 20)
      $fatal(1, "the array elements were %0d and %0d, expected 10 and 20",
             elements[0], elements[1]);

    if (into_dynamic !== 2)
      $fatal(1, "writing two dynamic elements returned %0d, expected 2",
             into_dynamic);
    if (dynamic_elements[0] !== 30 || dynamic_elements[1] !== 40)
      $fatal(1, "the dynamic elements were %0d and %0d, expected 30 and 40",
             dynamic_elements[0], dynamic_elements[1]);

    if (into_part_select !== 1)
      $fatal(1, "writing a part-select returned %0d, expected 1",
             into_part_select);
    if (packed_value !== 16'h00ab)
      $fatal(1, "the packed value was %h, expected 00ab", packed_value);

    if (into_members !== 2)
      $fatal(1, "writing two structure members returned %0d, expected 2",
             into_members);
    if (members.a !== 99 || members.b !== 77)
      $fatal(1, "the members were %0d and %0d, expected 99 and 77",
             members.a, members.b);
    $display("All checks passed");
  end
endmodule
