module Top;
  string s;
  string middle;
  string head;
  string full;

  initial begin
    s = "Hello";
    middle = s.substr(1, 3);
    head = s.substr(0, 1);
    full = s.substr(0, 4);
  end
endmodule
