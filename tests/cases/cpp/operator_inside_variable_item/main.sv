module Top;
  int var_item_match;
  int var_item_no_match;
  int var_range_match;
  int var_range_no_match;
  initial begin
    int v;
    int a;
    int b;
    int lo;
    int hi;
    a = 5; b = 7; v = 5;
    var_item_match = (v inside {a, b}) ? 1 : 0;
    v = 6;
    var_item_no_match = (v inside {a, b}) ? 1 : 0;
    lo = 10; hi = 20; v = 15;
    var_range_match = (v inside {[lo:hi]}) ? 1 : 0;
    v = 5;
    var_range_no_match = (v inside {[lo:hi]}) ? 1 : 0;
  end
endmodule
