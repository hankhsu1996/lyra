// An object's srandom seeds the generator its own randomize() draws from (LRM
// 18.13.3, 18.15), so seeding it again with the same value replays the values
// randomize() gave its random variables.
module Top;
  class Packet;
    rand int unsigned payload;
  endclass

  int unsigned first_payload, replayed_payload;

  initial begin
    Packet packet;
    packet = new;
    first_payload = 0;
    replayed_payload = 32'hFFFFFFFF;

    packet.srandom(42);
    if (packet.randomize() != 1) $fatal(1, "the first randomize() failed");
    first_payload = packet.payload;
    packet.srandom(42);
    if (packet.randomize() != 1) $fatal(1, "the second randomize() failed");
    replayed_payload = packet.payload;
  end

  final begin
    if (first_payload !== replayed_payload)
      $fatal(1, "a reseeded object randomized %0h then %0h", first_payload,
             replayed_payload);
    $display("All checks passed");
  end
endmodule
