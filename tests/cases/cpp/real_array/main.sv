module Top;
  // LRM 7.4 / Table 6-7: `real` is a value type, so it is a legal element of an
  // unpacked array (dynamic, queue, fixed). The OOB shield default is 0.0, and
  // a `map()` may produce a real element from a non-real source.
  real ra[] = '{1.5, 2.5, 3.0};
  real rq[$] = '{4.0, 5.0};
  real rf[2] = '{7.5, 8.5};
  real ra2[] = '{1.5, 2.5, 3.0};
  real written[];
  int n[] = '{2, 4};
  real scaled[];

  initial begin
    written = ra;
    written[1] = 9.0;
    rq.push_back(6.0);
    scaled = n.map(x) with (x * 0.5);
    $display("ra=%p", ra);
    $display("rq=%p", rq);
    $display("rf=%p", rf);
    $display("written=%p", written);
    $display("scaled=%p", scaled);
    // LRM 7.4.5: read on an invalid index returns the element default (0.0).
    $display("oob=%f", ra[99]);
    $display("eq=%b", ra == ra2);
  end
endmodule
