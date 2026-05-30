module Top;
  string a;
  string b;
  int n_hello;
  int n_empty;

  initial begin
    a = "hello";
    b = "";
    n_hello = a.len();
    n_empty = b.len();
  end
endmodule
