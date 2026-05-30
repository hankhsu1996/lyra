module Top;
  string s;
  byte neg;
  byte past_end;

  initial begin
    s = "hi";
    neg = s.getc(-1);
    past_end = s.getc(5);
  end
endmodule
