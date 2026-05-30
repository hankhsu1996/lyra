module Top;
  string a;
  string b;
  real x;
  real y;

  initial begin
    a = "3.14";
    b = "abc";
    x = a.atoreal();
    y = b.atoreal();
    $display("%f", x);
    $display("%f", y);
  end
endmodule
