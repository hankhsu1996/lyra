// A slice of a queue is assignment compatible with any unpacked array of as
// many elements, and writing one assigns each element of the source to the
// corresponding element of the slice in left-to-right order (LRM 7.6), the
// slice naming the elements a read of it yields: a bound below 0 counts as 0
// and one above $ counts as $, and a slice whose first bound lies past its last
// names no element (LRM 7.10.1), as does one whose bound holds x or z. The
// indexed forms name the same pair of bounds. That holds of a queue the
// procedure declares and of one the module declares, whose change wakes a
// process waiting on it (LRM 9.4.2); of the slice as an output argument,
// written as the task returns (LRM 13.5); and of the slice named as the memory
// a $readmem task loads, which is a queue addressed from 0 and keeps its size
// (LRM 21.4.1).
module Top;
  typedef int ints_t[$];
  typedef logic [7:0] bytes_t[$];

  int shared [$] = '{0, 1, 2, 3, 4, 5};
  bit woke;

  logic [7:0] loaded [$] = '{0, 0, 0, 0, 0, 0};
  int fd;
  int at;
  logic [31:0] unknown_bound;

  task automatic give_pair(output int pair_out [$]);
    pair_out = '{40, 41};
  endtask

  initial begin
    wait (shared[2] == 11);
    woke = 1;
  end

  initial begin
    automatic int local_q [$] = '{0, 1, 2, 3, 4, 5};
    automatic int pair [$] = '{10, 11};
    automatic int fixed_pair [2] = '{20, 21};
    automatic int none [$];

    local_q[2:3] = pair;
    if (local_q != ints_t'{0, 1, 10, 11, 4, 5})
      $fatal(1, "a slice of a local queue took %p, expected 10 and 11 at 2 and 3",
             local_q);

    local_q[4:9] = fixed_pair;
    if (local_q != ints_t'{0, 1, 10, 11, 20, 21})
      $fatal(1, "a slice past the end took %p, expected 20 and 21 at 4 and 5",
             local_q);

    local_q[-2:0] = '{30};
    if (local_q != ints_t'{30, 1, 10, 11, 20, 21})
      $fatal(1, "a slice before the start took %p, expected 30 at 0", local_q);

    local_q[3:1] = none;
    if (local_q != ints_t'{30, 1, 10, 11, 20, 21})
      $fatal(1, "a slice naming no element changed the queue to %p", local_q);

    unknown_bound = 'x;
    local_q[unknown_bound:2] = none;
    if (local_q != ints_t'{30, 1, 10, 11, 20, 21})
      $fatal(1, "a slice with an unknown bound changed the queue to %p", local_q);

    at = 1;
    local_q[at+:2] = pair;
    if (local_q != ints_t'{30, 10, 11, 11, 20, 21})
      $fatal(1, "an indexed-up slice took %p, expected 10 and 11 at 1 and 2",
             local_q);
    local_q[at+3-:2] = fixed_pair;
    if (local_q != ints_t'{30, 10, 11, 20, 21, 21})
      $fatal(1, "an indexed-down slice took %p, expected 20 and 21 at 3 and 4",
             local_q);

    give_pair(local_q[0:1]);
    if (local_q != ints_t'{40, 41, 11, 20, 21, 21})
      $fatal(1, "a slice as an output argument took %p, expected 40 and 41 at 0 and 1",
             local_q);

    #1;
    shared[1:2] = pair;
    #1;
    if (shared != ints_t'{0, 10, 11, 3, 4, 5})
      $fatal(1, "a slice of a module's queue took %p, expected 10 and 11 at 1 and 2",
             shared);
    if (!woke)
      $fatal(1, "writing a slice of a queue did not wake a process waiting on it");

    shared[4:9] = fixed_pair;
    if (shared != ints_t'{0, 10, 11, 3, 20, 21})
      $fatal(1, "a write past the end of a module's queue took %p", shared);

    fd = $fopen("three.hex", "w");
    $fwrite(fd, "0a 0b 0c\n");
    $fclose(fd);
    $readmemh("three.hex", loaded[2:5], 1, 3);
    if (loaded != bytes_t'{0, 0, 0, 8'h0a, 8'h0b, 8'h0c})
      $fatal(1, "a slice of a queue loaded from its own address 1 left %p", loaded);
    $display("All checks passed");
  end
endmodule
