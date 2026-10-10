// The inside operator compares expressions that are not integral with the
// equality operator, and takes any singular expression on its left (LRM
// 11.4.13). A set mixing an integral and a real expression is compared as
// reals, so a member is not rounded to the left operand's type.
module Top;
  class Token;
  endclass

  real r;
  string s;
  int x;
  Token kept, stranger, none;

  logic real_in, real_out;
  logic real_in_range, real_outside_range;
  logic int_against_real_member, int_equal_to_real_member;
  logic string_in, string_out;
  logic string_in_range, string_outside_range;
  logic handle_in, handle_out, null_in;

  initial begin
    real_in = 1'b0;            r = 2.5; real_in = r inside {1.5, 2.5};
    real_out = 1'b1;           r = 2.5; real_out = r inside {1.5, 3.5};
    real_in_range = 1'b0;      r = 2.5; real_in_range = r inside {[2.0:3.0]};
    real_outside_range = 1'b1; r = 3.5;
    real_outside_range = r inside {[2.0:3.0]};

    int_against_real_member = 1'b1;  x = 2;
    int_against_real_member = x inside {2.4, 7};
    int_equal_to_real_member = 1'b0; x = 2;
    int_equal_to_real_member = x inside {2.0, 7};

    string_in = 1'b0;            s = "b"; string_in = s inside {"a", "b"};
    string_out = 1'b1;           s = "b"; string_out = s inside {"a", "c"};
    string_in_range = 1'b0;      s = "b"; string_in_range = s inside {["a":"c"]};
    string_outside_range = 1'b1; s = "d";
    string_outside_range = s inside {["a":"c"]};

    kept = new;
    stranger = new;
    handle_in = 1'b0;  handle_in = kept inside {stranger, kept};
    handle_out = 1'b1; handle_out = kept inside {stranger, null};
    null_in = 1'b0;    null_in = none inside {stranger, null};
  end

  final begin
    if (real_in !== 1'b1) $fatal(1, "2.5 inside {1.5, 2.5} was %b", real_in);
    if (real_out !== 1'b0) $fatal(1, "2.5 inside {1.5, 3.5} was %b", real_out);
    if (real_in_range !== 1'b1)
      $fatal(1, "2.5 inside {[2.0:3.0]} was %b", real_in_range);
    if (real_outside_range !== 1'b0)
      $fatal(1, "3.5 inside {[2.0:3.0]} was %b", real_outside_range);
    if (int_against_real_member !== 1'b0)
      $fatal(1, "2 inside {2.4, 7} was %b", int_against_real_member);
    if (int_equal_to_real_member !== 1'b1)
      $fatal(1, "2 inside {2.0, 7} was %b", int_equal_to_real_member);
    if (string_in !== 1'b1) $fatal(1, "b inside {a, b} was %b", string_in);
    if (string_out !== 1'b0) $fatal(1, "b inside {a, c} was %b", string_out);
    if (string_in_range !== 1'b1)
      $fatal(1, "b inside {[a:c]} was %b", string_in_range);
    if (string_outside_range !== 1'b0)
      $fatal(1, "d inside {[a:c]} was %b", string_outside_range);
    if (handle_in !== 1'b1)
      $fatal(1, "a handle inside a set naming its object was %b", handle_in);
    if (handle_out !== 1'b0)
      $fatal(1, "a handle inside a set of others was %b", handle_out);
    if (null_in !== 1'b1)
      $fatal(1, "a null handle inside a set holding null was %b", null_in);
    $display("All checks passed");
  end
endmodule
