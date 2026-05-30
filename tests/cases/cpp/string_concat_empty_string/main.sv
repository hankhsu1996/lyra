module Top;
  string empty;
  string s;

  initial begin
    empty = "";
    s = {"prefix", empty, "suffix"};
  end
endmodule
