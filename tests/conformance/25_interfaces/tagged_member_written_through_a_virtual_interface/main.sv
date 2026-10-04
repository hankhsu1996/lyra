// A member of a tagged union is assigned with the usual dot notation, and the
// write is checked against the current tag (LRM 11.9). Where the union is
// reached through a virtual interface (LRM 25.9), the handle is part of the one
// target the source wrote, so an index the handle is selected by is evaluated
// once for the check and the write together. The same holds for a union a
// generate block reaches in the scope around it.
interface Bus;
  typedef union tagged packed {
    logic [7:0] A;
    logic [7:0] B;
  } choice_t;
  choice_t choice;
endinterface

module Top;
  Bus bus0 ();
  Bus bus1 ();
  virtual Bus handles [2];

  typedef union tagged packed {
    logic [7:0] A;
    logic [7:0] B;
  } choice_t;
  choice_t outer;

  int calls;

  function automatic int counted(int value);
    calls = calls + 1;
    return value;
  endfunction

  int on_write;
  logic [7:0] through_handle;
  logic [7:0] untouched;
  logic [7:0] from_block;

  if (1) begin : inner
    initial begin
      outer = tagged A 8'h01;
      outer.A = 8'h02;
      from_block = outer.A;
    end
  end

  initial begin
    handles[0] = bus0;
    handles[1] = bus1;
    bus0.choice = tagged A 8'h10;
    bus1.choice = tagged A 8'h20;

    calls = 0;
    handles[counted(1)].choice.A = 8'h21;
    on_write = calls;

    through_handle = bus1.choice.A;
    untouched = bus0.choice.A;
  end

  final begin
    if (through_handle !== 8'h21)
      $fatal(1, "the write through the handle left %0h, expected 21",
             through_handle);
    if (untouched !== 8'h10)
      $fatal(1, "the other interface was changed to %0h", untouched);
    if (from_block !== 8'h02)
      $fatal(1, "the write from the generate block left %0h, expected 2",
             from_block);
    if (on_write !== 1)
      $fatal(1, "the write ran the handle's index %0d times, expected 1",
             on_write);
    $display("All checks passed");
  end
endmodule
