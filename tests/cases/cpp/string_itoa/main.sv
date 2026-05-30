module Top;
  string s_pos;
  string s_neg;
  string s_zero;

  initial begin
    s_pos.itoa(123);
    s_neg.itoa(-42);
    s_zero.itoa(0);
  end
endmodule
