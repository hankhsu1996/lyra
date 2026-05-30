module Top;
  string a;
  string b;
  string c;
  int eq;
  int lt;
  int gt;

  initial begin
    a = "apple";
    b = "banana";
    c = "apple";
    eq = a.compare(c);
    lt = a.compare(b);
    gt = b.compare(a);
  end
endmodule
