// A parameter constant may be an aggregate -- an unpacked array or an
// unpacked structure. Such a parameter is assigned as a whole and no member of
// it may be assigned separately, but an individual member may be used in an
// expression, and a bit-select or part-select reaches into a member of
// integral type. The member is a constant wherever it appears, so it serves
// equally as the value of a procedural expression, of a continuous
// assignment, and of an elaboration-time expression such as a vector's range
// (LRM 6.20.2).
module Top;
  typedef struct {
    int width;
    int depth;
  } shape_t;

  localparam logic [31:0] WEIGHTS [4] = '{32'd10, 32'd20, 32'd30, 32'd40};
  localparam shape_t SHAPE = '{width: 8, depth: 3};

  logic [SHAPE.width-1:0] sized_by_member;

  int first_weight;
  int last_weight;
  int selected_weight;
  logic [7:0] low_byte;
  logic [3:0] high_nibble;
  int sum_of_two;
  int product_of_members;
  int depth_plus_one;
  int width_from_declaration;
  logic [31:0] copied [4];

  int continuous_first;
  int continuous_last;
  assign continuous_first = WEIGHTS[0];
  assign continuous_last = WEIGHTS[3];

  logic [31:0] bumped [4];
  for (genvar i = 0; i < 4; i++) begin : bump
    assign bumped[i] = WEIGHTS[i] + 32'd1;
  end

  initial begin
    int index;

    first_weight = WEIGHTS[0];
    last_weight = WEIGHTS[3];
    index = 2;
    selected_weight = WEIGHTS[index];

    low_byte = WEIGHTS[3][7:0];
    high_nibble = WEIGHTS[1][7:4];

    sum_of_two = WEIGHTS[1] + WEIGHTS[2];
    product_of_members = SHAPE.width * SHAPE.depth;
    depth_plus_one = SHAPE.depth + 1;

    copied = WEIGHTS;

    sized_by_member = '1;
    width_from_declaration = sized_by_member;
  end

  final begin
    if (first_weight !== 10)
      $fatal(1, "first_weight was %0d, expected 10", first_weight);
    if (last_weight !== 40)
      $fatal(1, "last_weight was %0d, expected 40", last_weight);
    if (selected_weight !== 30)
      $fatal(1, "selected_weight was %0d, expected 30", selected_weight);
    if (low_byte !== 8'h28)
      $fatal(1, "low_byte was %h, expected 28", low_byte);
    if (high_nibble !== 4'h1)
      $fatal(1, "high_nibble was %h, expected 1", high_nibble);
    if (sum_of_two !== 50)
      $fatal(1, "sum_of_two was %0d, expected 50", sum_of_two);
    if (product_of_members !== 24)
      $fatal(1, "product_of_members was %0d, expected 24", product_of_members);
    if (depth_plus_one !== 4)
      $fatal(1, "depth_plus_one was %0d, expected 4", depth_plus_one);
    if (width_from_declaration !== 255)
      $fatal(1, "a vector ranged by a structure member held %0d, expected 255",
             width_from_declaration);

    if (copied[0] !== 32'd10)
      $fatal(1, "copied[0] was %0d, expected 10", copied[0]);
    if (copied[2] !== 32'd30)
      $fatal(1, "copied[2] was %0d, expected 30", copied[2]);

    if (continuous_first !== 10)
      $fatal(1, "continuous_first was %0d, expected 10", continuous_first);
    if (continuous_last !== 40)
      $fatal(1, "continuous_last was %0d, expected 40", continuous_last);

    if (bumped[0] !== 32'd11)
      $fatal(1, "bumped[0] was %0d, expected 11", bumped[0]);
    if (bumped[3] !== 32'd41)
      $fatal(1, "bumped[3] was %0d, expected 41", bumped[3]);
    $display("All checks passed");
  end
endmodule
