module Top;
  event e;
  int saw_in_slot;
  int saw_after_advance;

  initial begin
    saw_in_slot = 0;
    saw_after_advance = 0;
    #5;
    -> e;
    // Same simulation time as the trigger: LRM 15.5.3 says `e.triggered`
    // is true throughout the time step in which the trigger occurred.
    if (e.triggered) saw_in_slot = 1;
    #5;
    // Simulation time has advanced; the triggered state is cleared.
    if (!e.triggered) saw_after_advance = 1;
  end
endmodule
