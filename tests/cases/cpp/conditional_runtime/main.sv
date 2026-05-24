module Top;
  int a;
  int b;
  initial begin
    int yes;
    int no_;
    yes = 5;
    no_ = 0;
    if (yes) a = 1;
    else     a = 2;
    if (no_) b = 1;
    else     b = 2;
  end
endmodule
