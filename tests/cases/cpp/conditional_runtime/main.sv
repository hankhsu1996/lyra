module Top;
  initial begin
    int yes;
    int no_;
    int a;
    int b;
    yes = 5;
    no_ = 0;
    if (yes) a = 1;
    else     a = 2;
    if (no_) b = 1;
    else     b = 2;
    $display("a=%0d", a);
    $display("b=%0d", b);
  end
endmodule
