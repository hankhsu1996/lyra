// A thread's RNG is seeded with the next random value of the thread that
// created it, and "each thread is seeded with a unique value, determined
// solely by its parent" (LRM 18.14.1, 18.14.2). So however many threads one
// parent creates, no two of them draw the same sequence: here a hundred
// thousand, each recording its first two draws as one 64-bit value, and every
// one of those values is different. A seed of 32 bits cannot do that: among
// this many it is expected to hand two threads the same one.
module Top;
  localparam int Threads = 100000;

  bit seen[longint unsigned];
  int repeated;
  int finished;

  initial begin
    for (int i = 0; i < Threads; i++) begin
      fork
        begin
          longint unsigned drawn;
          drawn[63:32] = $urandom;
          drawn[31:0] = $urandom;
          if (seen.exists(drawn)) repeated++;
          seen[drawn] = 1;
          finished++;
        end
      join_none
    end
    #1;

    if (finished !== Threads)
      $fatal(1, "%0d threads finished, expected %0d", finished, Threads);
    if (repeated !== 0)
      $fatal(1, "%0d threads drew what another thread of the same parent drew",
             repeated);
    $display("All checks passed");
  end
endmodule
