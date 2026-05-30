module Top;
  string a;
  string b;
  string c;
  string d;
  string e;
  integer plain;
  integer with_underscore;
  integer no_digits;
  integer stop_at_letter;
  integer negative;

  initial begin
    a = "123";
    b = "12_34";
    c = "abc";
    d = "42xyz";
    e = "-7";
    plain = a.atoi();
    with_underscore = b.atoi();
    no_digits = c.atoi();
    stop_at_letter = d.atoi();
    negative = e.atoi();
  end
endmodule
