module Top;
  int bit_plus_int;
  longint int_plus_longint;
  longint bit_plus_longint;
  bit byte_lt_int;
  bit byte_gt_int;
  bit ubyte_lt_int;
  bit ubyte_gt_int;
  initial begin
    bit a1;
    int b1;
    int a2;
    longint b2;
    bit a3;
    longint b3;
    byte sa;
    int sb;
    bit [7:0] ua;
    int ub;

    a1 = 1; b1 = 10;
    bit_plus_int = a1 + b1;

    a2 = 100; b2 = 1000;
    int_plus_longint = a2 + b2;

    a3 = 1; b3 = 1000;
    bit_plus_longint = a3 + b3;

    sa = -1;
    sb = 100;
    byte_lt_int = (sa < sb);
    byte_gt_int = (sa > sb);

    ua = 8'hFF;
    ub = 100;
    ubyte_lt_int = (ua < ub);
    ubyte_gt_int = (ua > ub);
  end
endmodule
