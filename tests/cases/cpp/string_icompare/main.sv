module Top;
  string a;
  string b;
  string c;
  int case_diff_eq;
  int diff_lt;

  initial begin
    a = "Hello";
    b = "hello";
    c = "world";
    case_diff_eq = a.icompare(b);
    diff_lt = a.icompare(c);
  end
endmodule
