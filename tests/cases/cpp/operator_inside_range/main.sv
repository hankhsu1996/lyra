module Top;
  int in_range;
  int at_lo;
  int at_hi;
  int just_below_lo;
  int just_above_hi;
  int range_no_match;
  int mixed_value_hits;
  int mixed_range_hits;
  int mixed_no_match;
  int multi_range_hits_second;
  initial begin
    int v;
    v = 5;  in_range = (v inside {[1:10]}) ? 1 : 0;
    v = 1;  at_lo = (v inside {[1:10]}) ? 1 : 0;
    v = 10; at_hi = (v inside {[1:10]}) ? 1 : 0;
    v = 0;  just_below_lo = (v inside {[1:10]}) ? 1 : 0;
    v = 11; just_above_hi = (v inside {[1:10]}) ? 1 : 0;
    v = 15; range_no_match = (v inside {[1:10]}) ? 1 : 0;
    v = 1;  mixed_value_hits = (v inside {1, [5:10], 20}) ? 1 : 0;
    v = 7;  mixed_range_hits = (v inside {1, [5:10], 20}) ? 1 : 0;
    v = 30; mixed_no_match = (v inside {1, [5:10], 20}) ? 1 : 0;
    v = 25; multi_range_hits_second = (v inside {[1:5], [20:30]}) ? 1 : 0;
  end
endmodule
