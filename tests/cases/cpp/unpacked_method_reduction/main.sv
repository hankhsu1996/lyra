module Top;
  // LRM 7.12.3 reduction methods on a fixed-size unpacked array receiver.
  // `byte` elements make the width-cast case meaningful: by default the result
  // is element-shaped, and a `with (int'(item))` clause widens it to 32 bits
  // (LRM 7.12.3).
  byte ab [4] = '{1, 2, 3, 4};
  int s;
  int p;
  int x;
  int sw;
  int idx_sum;
  int asrc [4] = '{4, 1, 3, 2};

  initial begin
    s = ab.sum;
    p = ab.product;
    x = ab.xor;
    sw = ab.sum with (int'(item));
    // LRM 7.12.4 `item.index`: 4*0 + 1*1 + 3*2 + 2*3 = 13.
    idx_sum = asrc.sum with (item * item.index);
  end
endmodule
