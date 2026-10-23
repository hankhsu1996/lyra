// An object's srandom seeds the generator its own randomize() draws from (LRM
// 18.13.3, 18.15), so seeding it again with the same value replays the values
// randomize() gave its random variables. And what it draws depends on that
// generator alone (LRM 18.14): another object randomized in between, of the
// same class and with the same constraints, changes none of it.
module Top;
  class Packet;
    rand int unsigned payload;
  endclass

  class Window;
    rand bit [7:0] low;
    rand bit [7:0] high;
    constraint ordered { low < high; high - low < 40; }
  endclass

  int unsigned first_payload, replayed_payload;
  bit [7:0] alone[8], interleaved[8];

  initial begin
    Packet packet;
    Window window, other;
    packet = new;
    first_payload = 0;
    replayed_payload = 32'hFFFFFFFF;

    packet.srandom(42);
    if (packet.randomize() != 1) $fatal(1, "the first randomize() failed");
    first_payload = packet.payload;
    packet.srandom(42);
    if (packet.randomize() != 1) $fatal(1, "the second randomize() failed");
    replayed_payload = packet.payload;

    window = new;
    other = new;
    window.srandom(7);
    for (int i = 0; i < 8; i++) begin
      if (window.randomize() != 1) $fatal(1, "a window randomize() failed");
      alone[i] = window.low;
    end
    window.srandom(7);
    for (int i = 0; i < 8; i++) begin
      if (other.randomize() != 1) $fatal(1, "the other randomize() failed");
      if (window.randomize() != 1) $fatal(1, "a window randomize() failed");
      interleaved[i] = window.low;
    end
  end

  final begin
    if (first_payload !== replayed_payload)
      $fatal(1, "a reseeded object randomized %0h then %0h", first_payload,
             replayed_payload);
    for (int i = 0; i < 8; i++)
      if (alone[i] !== interleaved[i])
        $fatal(1, "draw %0d was %0d alone and %0d beside another object", i,
               alone[i], interleaved[i]);
    $display("All checks passed");
  end
endmodule
