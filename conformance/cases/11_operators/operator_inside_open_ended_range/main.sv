// A bound of a value range written as $ stands for the lowest or the highest
// value of the type of the expression on the left of inside, so the range
// holds everything on that side of its other bound (LRM 11.4.13).
module Top;
  int x;
  logic [3:0] u;
  real r;
  string s;

  logic at_most_in, at_most_out;
  logic at_least_in, at_least_out;
  logic negative_at_most;
  logic unsigned_at_least;
  logic real_at_least_in, real_at_least_out;
  logic string_at_least_in, string_at_least_out;
  logic beside_a_value;
  logic unknown_left;

  int watched;
  wire continuously;
  assign continuously = watched inside {[10:$]};
  logic continuously_below, continuously_above;

  initial begin
    watched = 3;
    continuously_below = 1'b1;
    continuously_above = 1'b0;
    #1 continuously_below = continuously;
    watched = 12;
    #1 continuously_above = continuously;
  end

  initial begin
    at_most_in = 1'b0;   x = 5; at_most_in = x inside {[$:5]};
    at_most_out = 1'b1;  x = 6; at_most_out = x inside {[$:5]};
    at_least_in = 1'b0;  x = 5; at_least_in = x inside {[5:$]};
    at_least_out = 1'b1; x = 4; at_least_out = x inside {[5:$]};

    negative_at_most = 1'b0;  x = -2000000000;
    negative_at_most = x inside {[$:0]};
    unsigned_at_least = 1'b0; u = 4'd15;
    unsigned_at_least = u inside {[4'd10:$]};

    real_at_least_in = 1'b0;    r = 1.0e30;
    real_at_least_in = r inside {[0.5:$]};
    real_at_least_out = 1'b1;   r = 0.25;
    real_at_least_out = r inside {[0.5:$]};
    string_at_least_in = 1'b0;  s = "zz";
    string_at_least_in = s inside {["m":$]};
    string_at_least_out = 1'b1; s = "a";
    string_at_least_out = s inside {["m":$]};

    beside_a_value = 1'b0; x = 100;
    beside_a_value = x inside {3, [50:$]};

    // The bound that is written still compares, so an unknown left operand
    // leaves the answer unknown.
    unknown_left = 1'b0; u = 4'bx000;
    unknown_left = u inside {[$:4'd5]};
  end

  final begin
    if (at_most_in !== 1'b1) $fatal(1, "5 inside {[$:5]} was %b", at_most_in);
    if (at_most_out !== 1'b0) $fatal(1, "6 inside {[$:5]} was %b", at_most_out);
    if (at_least_in !== 1'b1) $fatal(1, "5 inside {[5:$]} was %b", at_least_in);
    if (at_least_out !== 1'b0)
      $fatal(1, "4 inside {[5:$]} was %b", at_least_out);
    if (negative_at_most !== 1'b1)
      $fatal(1, "-2000000000 inside {[$:0]} was %b", negative_at_most);
    if (unsigned_at_least !== 1'b1)
      $fatal(1, "15 inside {[10:$]} was %b", unsigned_at_least);
    if (real_at_least_in !== 1'b1)
      $fatal(1, "1.0e30 inside {[0.5:$]} was %b", real_at_least_in);
    if (real_at_least_out !== 1'b0)
      $fatal(1, "0.25 inside {[0.5:$]} was %b", real_at_least_out);
    if (string_at_least_in !== 1'b1)
      $fatal(1, "zz inside {[m:$]} was %b", string_at_least_in);
    if (string_at_least_out !== 1'b0)
      $fatal(1, "a inside {[m:$]} was %b", string_at_least_out);
    if (beside_a_value !== 1'b1)
      $fatal(1, "100 inside {3, [50:$]} was %b", beside_a_value);
    if (unknown_left !== 1'bx)
      $fatal(1, "x000 inside {[$:5]} was %b", unknown_left);
    if (continuously_below !== 1'b0)
      $fatal(1, "a continuous assignment of 3 inside {[10:$]} was %b",
             continuously_below);
    if (continuously_above !== 1'b1)
      $fatal(1, "a continuous assignment of 12 inside {[10:$]} was %b",
             continuously_above);
    $display("All checks passed");
  end
endmodule
