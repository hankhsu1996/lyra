module Top;
  // LRM 21.2.2: $strobe displays at the end of the current time step, after all
  // events have occurred and just before time advances -- so it reports each
  // argument's final value at that step, not its value when $strobe was called.
  // The trailing write to x is therefore the value observed: $strobe prints 99,
  // not 5. The local must be static (LRM 6.21) so it is still live to be read in
  // the postponed region; a $strobe of an automatic local is rejected.
  initial begin
    int x;
    x = 5;
    $strobe("%0d", x);
    x = 99;
  end
endmodule
