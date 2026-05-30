module Top;
  string s;
  int r;

  initial begin
    r = 7;
    s = "nope";
    case (s)
      "a": r = 1;
      "b": r = 2;
    endcase
  end
endmodule
