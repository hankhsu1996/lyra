module Top;
  int outer[2];
  int inner[2];

  int inner_uses_outer_element;
  int inner_uses_outer_index;
  int shadow_default;

  initial begin
    outer = '{1, 2};
    inner = '{10, 20};

    // LRM 7.12.4: the inner clause reads the outer element `x`, captured into
    // the inner closure. x=1: 1+(11+21)=33; x=2: 2+(12+22)=36; total 69.
    inner_uses_outer_element = outer.sum(x) with (x + inner.sum(y) with (y + x));

    // The inner clause reads the outer index `x.index`.
    // x.index=0: 10+20=30; x.index=1: 11+21=32; total 62.
    inner_uses_outer_index =
        outer.sum(x) with (inner.sum(y) with (y + x.index));

    // Default `item` in both clauses: the inner `item` shadows the outer one.
    // x=1: 1+(10+20)=31; x=2: 2+30=32; total 63.
    shadow_default = outer.sum() with (item + inner.sum() with (item));
  end
endmodule
