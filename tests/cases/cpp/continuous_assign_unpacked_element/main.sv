// A continuous assignment whose right-hand side selects an unpacked-array
// element (LRM 7.4.5 in a structural context, LRM 10.3).
module Top;
  int ua[2];
  int usum;
  assign usum = ua[0] + ua[1];

  initial begin
    ua[0] = 3;
    ua[1] = 4;
    #5;
    ua[0] = 10;
  end
endmodule
