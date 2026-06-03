module Top;
  int arr [4] = '{1, 2, 3, 4};
  int result;
  int trailing_marker;

  function int find_first_ge (int threshold);
    foreach (arr[i]) begin
      if (arr[i] >= threshold) return arr[i];
    end
    return -1;
  endfunction

  initial begin
    trailing_marker = 0;
    // First match for >= 3 is arr[2] = 3. return inside foreach must exit the
    // enclosing function, not just the foreach. trailing_marker stays 0 only
    // because no fall-through happened; with an IIFE-style wrapper, return
    // would have exited the lambda and trailing_marker would have flipped.
    result = find_first_ge(3);
  end
endmodule
