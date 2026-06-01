module Top;
  real r;
  real prefix_new;
  real postfix_old;
  initial begin
    r = 2.5;
    prefix_new = ++r;
    postfix_old = r--;
    $display("%f", r);
    $display("%f", prefix_new);
    $display("%f", postfix_old);
  end
endmodule
