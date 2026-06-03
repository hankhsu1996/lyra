module Top;
  // LRM 21.2.2 + LRM 21.2.1.1: $strobe / $strobeb / $strobeh / $strobeo pick
  // the same default radix as their $display / $displayb / $displayh /
  // $displayo counterparts. All four submits land in the postponed queue and
  // drain in append order.
  initial begin
    $strobe (8'hAB);
    $strobeb(8'hAB);
    $strobeh(8'hAB);
    $strobeo(8'hAB);
  end
endmodule
