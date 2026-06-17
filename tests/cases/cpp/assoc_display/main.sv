module Top;
  int m [string];
  int n [int];

  initial begin
    // Insertion order differs from key order to show that %p prints in the
    // LRM 7.8 key ordering (string lexicographical, integral numerical).
    m["b"] = 2;
    m["a"] = 1;
    n[10] = 100;
    n[3] = 30;
    $display("m=%p", m);
    $display("n=%p", n);
  end
endmodule
