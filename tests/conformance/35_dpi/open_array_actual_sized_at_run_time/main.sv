// An open-array formal fixes its unsized dimensions at the call rather than at
// the declaration, and the rules that govern a dynamic array formal govern
// those dimensions too, so a dynamic array is an actual for an input open
// array (LRM 7.6, 35.5.6.1). Its reported range is its own: a dynamic array
// runs from 0 to its element count less one, and the count is whatever the
// program last gave it (LRM 20.7, Annex H.7.6). One imported subroutine
// therefore serves the same array at two different sizes within one run.
module Top;
  import "DPI-C" function int weigh(input byte data[]);
  import "DPI-C" function int bounds_of(input byte data[], input int which);

  byte grown[];
  int weight_of_three;
  int weight_of_two;
  int shape[5];

  initial begin
    weight_of_three = -1;
    weight_of_two = -1;
    for (int i = 0; i < 5; i++) shape[i] = -1000;

    // Each element is weighted by the index it was read under, so an element
    // reached through the wrong index changes the total rather than
    // cancelling out against another.
    grown = new[3];
    grown[0] = 5;
    grown[1] = 6;
    grown[2] = 7;
    weight_of_three = weigh(grown);

    for (int i = 0; i < 5; i++) shape[i] = bounds_of(grown, i);

    grown = new[2];
    grown[0] = 1;
    grown[1] = 2;
    weight_of_two = weigh(grown);
  end

  final begin
    if (weight_of_three !== 20)
      $fatal(1, "weight_of_three was %0d, expected 20", weight_of_three);
    if (weight_of_two !== 2)
      $fatal(1, "weight_of_two was %0d, expected 2", weight_of_two);
    if (shape[0] !== 1)
      $fatal(1, "dimension count was %0d, expected 1", shape[0]);
    if (shape[1] !== 0)
      $fatal(1, "left bound was %0d, expected 0", shape[1]);
    if (shape[2] !== 2)
      $fatal(1, "right bound was %0d, expected 2", shape[2]);
    if (shape[3] !== 3)
      $fatal(1, "size was %0d, expected 3", shape[3]);
    if (shape[4] !== -1)
      $fatal(1, "increment was %0d, expected -1", shape[4]);
    $display("All checks passed");
  end
endmodule
