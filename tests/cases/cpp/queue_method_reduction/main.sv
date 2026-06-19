module Top;
  // LRM 7.12.3 reduction methods on a queue receiver. `byte` elements make the
  // width-cast case meaningful: by default the result is element-shaped, and a
  // `with (int'(item))` clause widens it to 32 bits (LRM 7.12.3).
  byte qb [$] = '{1, 2, 3, 4};
  int s;
  int p;
  int x;
  int sw;
  int qidx_sum;
  int qsrc [$] = '{4, 1, 3, 2};

  initial begin
    s = qb.sum;
    p = qb.product;
    x = qb.xor;
    sw = qb.sum with (int'(item));
    // LRM 7.12.4 `item.index`: 4*0 + 1*1 + 3*2 + 2*3 = 13.
    qidx_sum = qsrc.sum with (item * item.index);
  end
endmodule
