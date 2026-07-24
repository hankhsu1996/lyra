module Top;
  int a;
  int b;
  int c_all;
  int d_one;
  initial begin
    int yes;
    int no_;
    int t1;
    int t2;
    int t3;
    yes = 5;
    no_ = 0;
    if (yes) a = 1;
    else     a = 2;
    if (no_) b = 1;
    else     b = 2;

    // Multi-condition if (LRM 12.4): the `&&&`-separated conditions form a
    // conjunction, so the branch is taken iff every condition is true.
    t1 = 3;
    t2 = 7;
    t3 = 9;
    if (t1 &&& t2 &&& t3) c_all = 1;
    else                  c_all = 2;
    if (t1 &&& no_ &&& t3) d_one = 1;
    else                   d_one = 2;
  end
endmodule
