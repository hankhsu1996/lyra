// An assignment pattern may be the left-hand side of an assignment, in the
// positional notation, and deconstructs the value assigned: each member
// expression takes the corresponding element of an array or member of a
// structure (LRM 10.9). Prefixed with a data type it is an assignment pattern
// expression whose type is that data type. The right-hand side is evaluated
// before any member is written, so a pattern naming on its left what its right
// reads rotates the values (LRM 10.9, `U'{c, a, b} = '{a+1, b+1, c+1}`). A
// member expression may itself be a pattern, taking a member that is an
// aggregate apart in turn, and the members need not be of one type.
module Top;
  typedef byte triple_t[3];
  typedef struct packed {
    logic [3:0] upper;
    logic [3:0] lower;
  } pair_t;
  typedef struct {
    int count;
    string label;
    triple_t bytes;
  } record_t;

  triple_t source_array;
  byte first, second, third;
  byte rotate_a, rotate_b, rotate_c;

  int plain_array[4];
  int untyped_a, untyped_b, untyped_c, untyped_d;

  pair_t source_pair;
  logic [3:0] pair_upper, pair_lower;

  record_t source_record;
  int record_count;
  string record_label;
  byte record_x, record_y, record_z;

  initial begin
    source_array = '{8'd11, 8'd22, 8'd33};
    first = 0;
    second = 0;
    third = 0;
    triple_t'{first, second, third} = source_array;

    rotate_a = 8'd1;
    rotate_b = 8'd2;
    rotate_c = 8'd3;
    triple_t'{rotate_c, rotate_a, rotate_b} =
        '{rotate_a + 8'd10, rotate_b + 8'd10, rotate_c + 8'd10};

    plain_array = '{100, 200, 300, 400};
    untyped_a = 0;
    untyped_b = 0;
    untyped_c = 0;
    untyped_d = 0;
    '{untyped_a, untyped_b, untyped_c, untyped_d} = plain_array;

    source_pair = 8'h6B;
    pair_upper = 4'h0;
    pair_lower = 4'h0;
    pair_t'{pair_upper, pair_lower} = source_pair;

    source_record = '{42, "answer", '{8'd7, 8'd8, 8'd9}};
    record_count = 0;
    record_label = "";
    record_x = 0;
    record_y = 0;
    record_z = 0;
    record_t'{record_count, record_label, triple_t'{record_x, record_y, record_z}} =
        source_record;
  end

  final begin
    if (first !== 8'd11 || second !== 8'd22 || third !== 8'd33)
      $fatal(1, "array members were %0d %0d %0d", first, second, third);
    if (rotate_c !== 8'd11 || rotate_a !== 8'd12 || rotate_b !== 8'd13)
      $fatal(1, "rotation left a=%0d b=%0d c=%0d", rotate_a, rotate_b, rotate_c);
    if (untyped_a !== 100 || untyped_b !== 200 || untyped_c !== 300 ||
        untyped_d !== 400)
      $fatal(1, "untyped pattern left %0d %0d %0d %0d", untyped_a, untyped_b,
             untyped_c, untyped_d);
    if (pair_upper !== 4'h6 || pair_lower !== 4'hB)
      $fatal(1, "packed members were %h %h", pair_upper, pair_lower);
    if (record_count !== 42) $fatal(1, "record_count was %0d", record_count);
    if (record_label != "answer")
      $fatal(1, "record_label was '%s'", record_label);
    if (record_x !== 8'd7 || record_y !== 8'd8 || record_z !== 8'd9)
      $fatal(1, "nested members were %0d %0d %0d", record_x, record_y, record_z);
    $display("All checks passed");
  end
endmodule
