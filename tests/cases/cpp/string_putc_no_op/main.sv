module Top;
  string s_zero;
  string s_oob;
  string s_neg;

  initial begin
    s_zero = "Hi";
    s_oob = "Hi";
    s_neg = "Hi";
    s_zero.putc(0, 8'h00);
    s_oob.putc(5, 8'h41);
    s_neg.putc(-1, 8'h41);
  end
endmodule
