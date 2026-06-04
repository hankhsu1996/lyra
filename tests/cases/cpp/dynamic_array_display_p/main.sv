module Top;
  int a [];
  int b [];
  int c [][];
  int d [2][];
  int e [][2];
  initial begin
    a = '{10, 20, 30};
    // b stays empty -- LRM Table 6-7 default for dynamic array.
    c = '{'{1, 2}, '{3, 4, 5}};
    d = '{'{100, 200, 300}, '{400}};
    e = '{'{7, 8}, '{9, 10}, '{11, 12}};
  end
endmodule
