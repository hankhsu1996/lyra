// A process's generator is reached through its handle (LRM 9.7): srandom
// restarts it from a seed (LRM 18.13.3), get_randstate reads its state out and
// set_randstate puts a state read that way back (LRM 18.13.4, 18.13.5), and the
// draws that process makes next continue from what was set. A seed names one
// generator whichever process it is given to, so a process seeded through
// another's handle draws what the seeding process drew from the same seed, and
// a state read from one process and set on another makes the second draw what
// the first does.
module Top;
  int unsigned seeded_first, seeded_again;
  int unsigned restored_first, restored_again;
  int unsigned seeded_through_handle;
  int unsigned moved_here, moved_there;

  process first_child;
  process second_child;

  initial begin
    process self_handle;
    string kept;
    int unsigned skipped;

    seeded_first = 0;
    seeded_again = 32'hFFFFFFFF;
    restored_first = 0;
    restored_again = 32'hFFFFFFFF;
    seeded_through_handle = 32'hFFFFFFFF;
    moved_here = 0;
    moved_there = 32'hFFFFFFFF;

    self_handle = process::self();
    self_handle.srandom(11);
    seeded_first = $urandom;
    self_handle.srandom(11);
    seeded_again = $urandom;

    kept = self_handle.get_randstate();
    restored_first = $urandom;
    skipped = $urandom;
    self_handle.set_randstate(kept);
    restored_again = $urandom;

    fork
      begin
        first_child = process::self();
        #2;
        seeded_through_handle = $urandom;
      end
    join_none
    #1;
    first_child.srandom(11);
    #2;

    fork
      begin
        second_child = process::self();
        #2;
        moved_there = $urandom;
      end
    join_none
    #1;
    kept = self_handle.get_randstate();
    second_child.set_randstate(kept);
    moved_here = $urandom;
    #2;
  end

  final begin
    if (seeded_first !== seeded_again)
      $fatal(1, "a reseeded process drew %0h then %0h", seeded_first,
             seeded_again);
    if (restored_first !== restored_again)
      $fatal(1, "a restored process drew %0h then %0h", restored_first,
             restored_again);
    if (seeded_through_handle !== seeded_first)
      $fatal(1, "a process seeded through its handle drew %0h, expected %0h",
             seeded_through_handle, seeded_first);
    if (moved_there !== moved_here)
      $fatal(1, "a process given another's state drew %0h, expected %0h",
             moved_there, moved_here);
    $display("All checks passed");
  end
endmodule
