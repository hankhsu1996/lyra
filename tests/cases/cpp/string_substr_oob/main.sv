module Top;
  string s;
  string j_lt_i;
  string past_end;
  string neg_i;

  initial begin
    s = "Hello";
    j_lt_i = s.substr(3, 1);
    past_end = s.substr(0, 10);
    neg_i = s.substr(-1, 2);
  end
endmodule
