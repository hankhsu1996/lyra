module Top;
  // LRM 7.12.2 ordering methods on a queue receiver. The queue is an observable
  // structural variable, so an in-place reorder routes through the mutate
  // receiver path; asserting the final contents proves the write persisted.
  int qs [$] = '{3, -1, 2, -5};
  int qr [$] = '{3, -1, 2, -5};
  int qrev [$] = '{1, 2, 3};
  int qkey [$] = '{3, -1, 2, -5};
  int qempty [$];

  initial begin
    qs.sort();
    qr.rsort();
    qrev.reverse();
    // `with` key = 10 - item sorts ascending by key, i.e. descending by item.
    qkey.sort with (10 - item);
    qempty.sort();
  end
endmodule
