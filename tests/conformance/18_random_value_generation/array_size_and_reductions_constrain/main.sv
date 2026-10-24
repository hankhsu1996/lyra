// The size of a random dynamic array or queue is chosen where a constraint
// names it, before its elements are, and the array is resized to it (LRM
// 18.4, 18.5.7.1); a size may be named in `solve ... before`, every size the
// constraints allow leaves its elements an answer, and a soft constraint on a
// size holds where the hard ones allow it (LRM 18.5.13). A reduction of an array
// -- sum(), xor(), with or without a `with` clause naming the element and its
// index -- is an expression over its elements a constraint can hold (LRM
// 18.5.7.2).
module Top;
  class Arrays;
    rand bit [7:0] data[];
    rand bit [7:0] tail[$];
    rand int count;
    rand bit [3:0] lanes[4];
    rand bit [7:0] preferred[];
    constraint sized {
      data.size() == count;
      count inside {[2:5]};
      foreach (data[i]) data[i] >= 10;
    }
    constraint queued {
      tail.size() > 0;
      tail.size() < 3;
      solve tail.size() before count;
      foreach (tail[i]) tail[i] > 200;
    }
    constraint preferred_size {
      soft preferred.size() == 3;
      soft preferred.size() == 40;
      preferred.size() < 10;
    }
    constraint summed {
      lanes.sum() with (int'(item)) == 30;
      foreach (lanes[i]) lanes[i] > 0;
    }
    constraint weighted {
      data.sum() with (int'(item) * (item.index + 1)) < 1000;
      data.xor() != 0;
    }
  endclass

  int calls;
  int violations;
  int sizes_seen[6];

  initial begin
    Arrays a;
    a = new;
    calls = -1;
    violations = -1;

    calls = 0;
    violations = 0;
    for (int n = 0; n < 100; n++) begin
      int sum;
      bit [7:0] parity;
      calls += a.randomize();
      if (a.data.size() != a.count) violations++;
      foreach (a.data[i]) if (a.data[i] < 10) violations++;
      if (a.tail.size() < 1 || a.tail.size() > 2) violations++;
      foreach (a.tail[i]) if (a.tail[i] <= 200) violations++;
      sum = 0;
      foreach (a.lanes[i]) begin
        sum += a.lanes[i];
        if (a.lanes[i] == 0) violations++;
      end
      if (sum != 30) violations++;
      parity = 0;
      foreach (a.data[i]) parity ^= a.data[i];
      if (parity == 0) violations++;
      if (a.count >= 0 && a.count <= 5) sizes_seen[a.count]++;
      if (a.preferred.size() != 3) violations++;
    end
  end

  final begin
    if (calls !== 100) $fatal(1, "%0d of 100 calls succeeded", calls);
    if (violations !== 0) $fatal(1, "%0d constraint violations", violations);
    for (int s = 2; s <= 5; s++)
      if (sizes_seen[s] === 0) $fatal(1, "a size of %0d was never chosen", s);
    $display("All checks passed");
  end
endmodule
