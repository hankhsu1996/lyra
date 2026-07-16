`timescale 1ns / 1ns
module Test;
  // LRM 9.7 fine-grain process control: `kill()` forcibly terminates a process
  // (reported KILLED and removed from every wait it was parked on), and
  // `await()` suspends the caller until a process terminates normally
  // (reported FINISHED). One process forks two workers -- a long-lived one it
  // kills while parked, and a short one it awaits.
  int killed_ok;
  int await_ordered;
  int finished_ok;

  process slow;
  process quick;
  int quick_marker;

  initial begin
    fork
      begin
        slow = process::self();
        #100;
      end
      begin
        quick = process::self();
        #10;
        quick_marker = 7;
      end
    join_none

    #1;

    slow.kill();
    killed_ok = (slow.status() == process::KILLED);

    quick.await();
    await_ordered = (quick_marker == 7);
    finished_ok = (quick.status() == process::FINISHED);
  end
endmodule
