// An element of an array an automatic body declares (LRM 6.21) is reached
// where it lies, for a read and for a write. A read through an index naming no
// element answers the element default -- for a structure element, each
// member's declared value (LRM 7.4.6, Table 7-1, 7.2.2), also where the read
// goes on into a member of that element -- and allocates nothing. A write
// through one is ignored. An associative array allocates an entry for a write
// and for a read-modify-write, which starts from the array's default, and for
// nothing else (LRM 7.8.6, 7.8.7). An element is a receiver a method changing
// its receiver changes, and an element written in place leaves its neighbours
// alone.
module Top;
  typedef struct {
    int f[2] = '{5, 5};
    int g;
  } holder_t;

  typedef struct {
    int x = 1;
    int y = 2;
  } xy_t;

  function automatic void check(int nowhere);
    holder_t fixed[4];
    holder_t dynamic[];
    holder_t queue[$];
    int ints[3];
    int counted[int] = '{default: 1};
    xy_t pairs[int];
    int rows[$][$];
    int read_value;

    read_value = fixed[nowhere].f[0];
    if (read_value !== 5)
      $fatal(1, "fixed[nowhere].f[0] read %0d, expected 5", read_value);
    read_value = dynamic[2].f[1];
    if (read_value !== 5)
      $fatal(1, "dynamic[2].f[1] read %0d, expected 5", read_value);
    read_value = queue[0].f[0];
    if (read_value !== 5)
      $fatal(1, "queue[0].f[0] read %0d, expected 5", read_value);
    if (dynamic.size() !== 0 || queue.size() !== 0)
      $fatal(1, "a read through no element sized dynamic %0d, queue %0d",
             dynamic.size(), queue.size());

    ints = '{10, 20, 30};
    ints[nowhere] = 99;
    ints[1] += 5;
    if (ints[0] !== 10 || ints[1] !== 25 || ints[2] !== 30)
      $fatal(1, "ints was %p, expected '{10, 25, 30}", ints);
    fixed[1].f[1] = 8;
    if (fixed[1].f[1] !== 8 || fixed[1].f[0] !== 5 || fixed[0].f[1] !== 5)
      $fatal(1, "fixed[1].f[1] = 8 left %p", fixed);

    read_value = counted[4];
    if (read_value !== 1 || counted.num() !== 0)
      $fatal(1, "counted[4] read %0d and left %0d entries, expected 1 and 0",
             read_value, counted.num());
    counted[1]++;
    if (counted[1] !== 2 || counted.num() !== 1)
      $fatal(1, "counted[1]++ left %0d in %0d entries, expected 2 in 1",
             counted[1], counted.num());
    pairs[2].x = 5;
    if (pairs[2].x !== 5 || pairs[2].y !== 2 || pairs.num() !== 1)
      $fatal(1, "pairs[2].x = 5 left x %0d, y %0d in %0d entries",
             pairs[2].x, pairs[2].y, pairs.num());

    rows = '{'{1}, '{2}};
    rows[1].push_back(3);
    if (rows[1].size() !== 2 || rows[1][1] !== 3 || rows[0].size() !== 1)
      $fatal(1, "rows[1].push_back(3) left %p", rows);
  endfunction

  initial begin
    check(9);
    $display("All checks passed");
  end
endmodule
