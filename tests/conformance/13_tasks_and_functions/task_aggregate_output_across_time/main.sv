// A task's output formal of an unpacked structure or array type is copied
// back to its actual when the task completes, whole, however long the task
// took; each enable copies back its own, so a task enabled once per iteration
// of a loop hands each iteration the value that enable produced (LRM 13.3,
// 13.5).
module Top;
  typedef struct {
    int n;
    string s;
  } pair_t;

  task automatic make(input int i, output pair_t got, output int arr[2]);
    #1;
    got = '{i, $sformatf("v%0d", i)};
    arr = '{i, i * 10};
  endtask

  int seen_n[3];
  string seen_s[3];
  int sums[3];

  initial begin
    for (int i = 0; i < 3; i++) begin
      pair_t r;
      int a[2];
      make(i + 1, r, a);
      seen_n[i] = r.n;
      seen_s[i] = r.s;
      sums[i] = a[0] + a[1];
    end
  end

  final begin
    for (int i = 0; i < 3; i++) begin
      if (seen_n[i] !== i + 1)
        $fatal(1, "seen_n[%0d] was %0d, expected %0d", i, seen_n[i], i + 1);
      if (seen_s[i] != $sformatf("v%0d", i + 1))
        $fatal(1, "seen_s[%0d] was %s", i, seen_s[i]);
      if (sums[i] !== (i + 1) * 11)
        $fatal(1, "sums[%0d] was %0d, expected %0d", i, sums[i], (i + 1) * 11);
    end
    $display("All checks passed");
  end
endmodule
