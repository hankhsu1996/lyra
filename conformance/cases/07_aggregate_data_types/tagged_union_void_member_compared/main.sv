// A tagged union member declared void contains nothing, so a value holding it
// is its tag alone (LRM 7.3.2). Two values holding that member are then one
// value: equality and case equality hold between them and inequality does not,
// and a copy of one compares equal to what it copied, while a value holding
// another member is unequal to it (LRM 11.2.2, 11.4.5).
module Top;
  typedef union tagged {
    void Invalid;
    int  Valid;
  } VInt;

  VInt a;
  VInt b;
  VInt c;

  logic equal;
  logic case_equal;
  logic not_equal;
  logic copy_equal;
  logic other_member_equal;
  logic other_member_case_equal;

  initial begin
    // Every target starts at a value its own check rejects.
    equal = 1'b0;
    case_equal = 1'b0;
    not_equal = 1'b1;
    copy_equal = 1'b0;
    other_member_equal = 1'b1;
    other_member_case_equal = 1'b1;

    a = tagged Invalid;
    b = tagged Invalid;
    equal = (a == b);
    case_equal = (a === b);
    not_equal = (a != b);

    c = tagged Valid 5;
    c = a;
    copy_equal = (c == a);

    b = tagged Valid 0;
    other_member_equal = (a == b);
    other_member_case_equal = (a === b);
  end

  final begin
    if (equal !== 1'b1)
      $fatal(1, "two values holding the void member compared as %b", equal);
    if (case_equal !== 1'b1)
      $fatal(1, "=== of two values holding the void member was %b",
             case_equal);
    if (not_equal !== 1'b0)
      $fatal(1, "!= of two values holding the void member was %b",
             not_equal);
    if (copy_equal !== 1'b1)
      $fatal(1, "a copy holding the void member compared as %b", copy_equal);
    if (other_member_equal !== 1'b0)
      $fatal(1, "values holding different members compared as %b",
             other_member_equal);
    if (other_member_case_equal !== 1'b0)
      $fatal(1, "=== of values holding different members was %b",
             other_member_case_equal);
    $display("All checks passed");
  end
endmodule
