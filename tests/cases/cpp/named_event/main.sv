// Named events (LRM 15.5). e_basic covers the declare / await `@e` / trigger
// `-> e` round-trip. e1 and e2 are two independent events triggered at
// different times, each waking only its own waiter. e_bc broadcasts one
// trigger to three waiters, all of which wake. e_tr exercises LRM 15.5.3
// `e.triggered`: true throughout the time step of the trigger (in_slot), false
// once simulation time advances (after).
module Top;
  event e_basic;
  event e1, e2;
  event e_tr;
  event e_bc;

  int basic = 0;
  int w1 = 0;
  int w2 = 0;
  int in_slot = 0;
  int after = 0;
  int b1 = 0;
  int b2 = 0;
  int b3 = 0;

  initial begin
    @e_basic;
    basic = 1;
  end
  initial begin
    @e1;
    w1 = 1;
  end
  initial begin
    @e2;
    w2 = 2;
  end
  initial begin
    @e_bc;
    b1 = 1;
  end
  initial begin
    @e_bc;
    b2 = 2;
  end
  initial begin
    @e_bc;
    b3 = 3;
  end

  initial begin
    #5;
    -> e_basic;
    -> e_bc;
    -> e2;
    -> e_tr;
    if (e_tr.triggered) in_slot = 1;
    #5;
    -> e1;
    if (!e_tr.triggered) after = 1;
  end
endmodule
