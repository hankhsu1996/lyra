// A structure type declared in a package is one type wherever it is imported
// (LRM 6.22), and every operation the language defines on a whole structure
// applies to it in the importing module as it would where it was declared,
// whether or not the package itself ever holds a value of it: equality member
// by member, case equality, its size in bits, unknown detection, and the
// comparison of queues holding it (LRM 26.3, 7.2, 11.4.5, 20.6.2, 20.9).
module Top;
  import point_pkg::*;

  point_t a, b, c;
  point_t queue_a [$];
  point_t queue_b [$];

  logic equal_same;
  logic equal_other;
  logic case_equal_same;
  logic has_unknown;
  logic queues_equal;
  int width;

  initial begin
    equal_same = 1'b0;
    equal_other = 1'b1;
    case_equal_same = 1'b0;
    has_unknown = 1'b0;
    queues_equal = 1'b0;
    width = 0;

    a = '{x: 7, tag: 4'b0101};
    b = '{x: 7, tag: 4'b0101};
    c = '{x: 8, tag: 4'b01x1};

    equal_same = (a == b);
    equal_other = (a == c);
    case_equal_same = (a === b);
    has_unknown = $isunknown(c);
    width = $bits(a);

    queue_a.push_back(a);
    queue_a.push_back(c);
    queue_b.push_back(b);
    queue_b.push_back(c);
    queues_equal = (queue_a === queue_b);
  end

  final begin
    if (equal_same !== 1'b1)
      $fatal(1, "equal_same was %b, expected 1", equal_same);
    if (equal_other !== 1'b0)
      $fatal(1, "equal_other was %b, expected 0", equal_other);
    if (case_equal_same !== 1'b1)
      $fatal(1, "case_equal_same was %b, expected 1", case_equal_same);
    if (has_unknown !== 1'b1)
      $fatal(1, "has_unknown was %b, expected 1", has_unknown);
    if (width !== 36) $fatal(1, "width was %0d, expected 36", width);
    if (queues_equal !== 1'b1)
      $fatal(1, "queues_equal was %b, expected 1", queues_equal);
    $display("All checks passed");
  end
endmodule
