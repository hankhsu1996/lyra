module Top;
  int match_first;
  int match_middle;
  int match_last;
  int no_match_below;
  int no_match_between;
  int no_match_above;
  int single_item_match;
  int single_item_no_match;
  initial begin
    int v;
    v = 1; match_first = (v inside {1, 5, 9}) ? 1 : 0;
    v = 5; match_middle = (v inside {1, 5, 9}) ? 1 : 0;
    v = 9; match_last = (v inside {1, 5, 9}) ? 1 : 0;
    v = 0; no_match_below = (v inside {1, 5, 9}) ? 1 : 0;
    v = 3; no_match_between = (v inside {1, 5, 9}) ? 1 : 0;
    v = 12; no_match_above = (v inside {1, 5, 9}) ? 1 : 0;
    v = 42; single_item_match = (v inside {42}) ? 1 : 0;
    v = 42; single_item_no_match = (v inside {43}) ? 1 : 0;
  end
endmodule
