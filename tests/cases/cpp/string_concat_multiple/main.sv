module Top;
  string a;
  string b;
  string c;
  string s;
  string lit;

  initial begin
    a = "one";
    b = "two";
    c = "three";
    s = {a, "-", b, "-", c};
    // LRM 5.9 / 11.4.12.2: an all-literal concatenation types as a packed bit
    // vector, then converts to string on assignment to a string target.
    lit = {"foo", "bar"};
  end
endmodule
