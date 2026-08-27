// A formal argument that leaves a dimension unsized is an open array, which
// relaxes the argument-matching rule so that one imported subroutine serves
// actuals of any size and any range (LRM 35.5.6.1). The sizes and ranges are
// therefore fixed by the actual on a per-call basis, and the foreign side
// reads them back through the array querying functions, which follow the
// semantics of the SystemVerilog array querying functions: an unsized unpacked
// dimension keeps the actual's own declared bounds rather than a normalized
// form (LRM Annex H.7.6, H.12.1, H.12.2, 20.7).
module Top;
  import "DPI-C" function int weigh(input byte data[]);
  import "DPI-C" function int query(input byte data[], input int which);

  byte few[4];
  byte many[3:1];
  int few_weight;
  int many_weight;
  int few_shape[7];
  int many_shape[7];

  initial begin
    few[0] = 1;
    few[1] = 2;
    few[2] = 3;
    few[3] = 4;
    many[3] = 10;
    many[2] = 20;
    many[1] = 30;

    // Each element is weighted by the index it was read under, so an element
    // reached through the wrong index changes the total rather than
    // cancelling out against another.
    few_weight = weigh(few);
    many_weight = weigh(many);

    for (int i = 0; i < 7; i++) begin
      few_shape[i] = query(few, i);
      many_shape[i] = query(many, i);
    end
  end

  final begin
    if (few_weight !== 20)
      $fatal(1, "weighing few[0:3] gave %0d, expected 20", few_weight);
    if (many_weight !== 100)
      $fatal(1, "weighing many[3:1] gave %0d, expected 100", many_weight);

    if (few_shape[0] !== 1)
      $fatal(1, "few[0:3] reported %0d dimensions, expected 1", few_shape[0]);
    if (few_shape[1] !== 0)
      $fatal(1, "the left bound of few[0:3] was %0d, expected 0",
             few_shape[1]);
    if (few_shape[2] !== 3)
      $fatal(1, "the right bound of few[0:3] was %0d, expected 3",
             few_shape[2]);
    if (few_shape[3] !== 0)
      $fatal(1, "the low bound of few[0:3] was %0d, expected 0", few_shape[3]);
    if (few_shape[4] !== 3)
      $fatal(1, "the high bound of few[0:3] was %0d, expected 3",
             few_shape[4]);
    if (few_shape[5] !== 4)
      $fatal(1, "the size of few[0:3] was %0d, expected 4", few_shape[5]);
    if (few_shape[6] !== -1)
      $fatal(1, "the increment of few[0:3] was %0d, expected -1",
             few_shape[6]);

    if (many_shape[0] !== 1)
      $fatal(1, "many[3:1] reported %0d dimensions, expected 1",
             many_shape[0]);
    if (many_shape[1] !== 3)
      $fatal(1, "the left bound of many[3:1] was %0d, expected 3",
             many_shape[1]);
    if (many_shape[2] !== 1)
      $fatal(1, "the right bound of many[3:1] was %0d, expected 1",
             many_shape[2]);
    if (many_shape[3] !== 1)
      $fatal(1, "the low bound of many[3:1] was %0d, expected 1",
             many_shape[3]);
    if (many_shape[4] !== 3)
      $fatal(1, "the high bound of many[3:1] was %0d, expected 3",
             many_shape[4]);
    if (many_shape[5] !== 3)
      $fatal(1, "the size of many[3:1] was %0d, expected 3", many_shape[5]);
    if (many_shape[6] !== 1)
      $fatal(1, "the increment of many[3:1] was %0d, expected 1",
             many_shape[6]);
    $display("All checks passed");
  end
endmodule
