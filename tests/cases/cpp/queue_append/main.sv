// LRM 7.10.1 queue element-write rules. A write to `q[$+1]` (index == size) is
// a legal append; an index that is x/z, negative, or beyond `$+1` is ignored.
// A read of an invalid index returns the element default and never grows the
// queue, so the read/write split matters: `q[100]` read stays a no-op. The
// read-modify-write forms `q[i]++` and `q[i] += v` also take the write path so
// the update reaches the observable queue.
module Top;
  int q [$] = '{1, 2, 3};
  int last;
  int oob_read;
  int size_after;

  initial begin
    q[$+1] = 4;
    q[$+1] = 5;
    q[10] = 99;
    q[1] = 20;
    q[0]++;
    q[2] += 5;
    last = q[$];
    oob_read = q[100];
    size_after = q.size();
  end
endmodule
