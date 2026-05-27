module Top;
  int sum_one_match;
  int sum_both_match;
  int sum_no_match;
  int ternary_branch_taken;
  int ternary_branch_not_taken;
  initial begin
    int v;
    v = 3;
    sum_one_match = (v inside {1, 2, 3}) + (v inside {4, 5, 6});
    v = 5;
    sum_both_match = (v inside {[1:10]}) + (v inside {[3:7]});
    v = 50;
    sum_no_match = (v inside {1, 2, 3}) + (v inside {4, 5, 6});
    v = 5;
    ternary_branch_taken = (v inside {[1:10]}) ? 100 : 200;
    v = 50;
    ternary_branch_not_taken = (v inside {[1:10]}) ? 100 : 200;
  end
endmodule
