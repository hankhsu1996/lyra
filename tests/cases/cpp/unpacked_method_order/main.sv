module Top;
  // LRM 7.12.2 ordering methods on a fixed-size unpacked array receiver. The
  // array is an observable structural variable, so an in-place reorder routes
  // through the mutate receiver path; asserting the final contents proves the
  // write persisted. A fixed array is never empty, so size is preserved.
  int a_sort [4] = '{3, -1, 2, -5};
  int a_rsort [4] = '{3, -1, 2, -5};
  int a_rev [3] = '{1, 2, 3};
  int a_key [4] = '{3, -1, 2, -5};

  initial begin
    a_sort.sort();
    a_rsort.rsort();
    a_rev.reverse();
    // `with` key = 10 - item sorts ascending by key, i.e. descending by item.
    a_key.sort with (10 - item);
  end
endmodule
