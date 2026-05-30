module Top;
  string a;
  string b;
  string c;
  string d;
  string inner;
  string s;

  initial begin
    a = "a";
    b = "b";
    c = "c";
    d = "d";
    inner = {b, "-", c};
    s = {a, "-", inner, "-", d};
  end
endmodule
