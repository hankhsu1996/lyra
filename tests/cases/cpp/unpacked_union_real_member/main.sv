module Top;
  typedef union {
    int       i;
    shortreal f;
  } num_t;

  num_t n;
  int   i_rt;
  int   f_ok;

  initial begin
    n.i = 9;
    i_rt = n.i;

    n.f = 2.5;
    f_ok = (n.f == 2.5);
  end
endmodule
