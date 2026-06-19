module Top;
  // LRM 7.12.5: map() returns an unpacked array with the same shape and index
  // type as the source, each element replaced by the mandatory with-expression.
  // The result element type is the self-determined type of that expression, so
  // it may differ from the source element type. The result container kind
  // follows the receiver: dynamic -> dynamic, queue -> queue, fixed -> fixed.
  int a[] = '{1, 2, 3};
  int b[] = '{10, 20, 30};
  int q[$] = '{4, 5};
  int fa[3] = '{7, 8, 9};
  int empty[];
  int m[][] = '{'{1, 2, 3}, '{4, 5}};

  int sum_ab[];
  bit gt[];
  int qd[$];
  int fad[3];
  int em[];
  int rowsum[];

  initial begin
    // LRM 7.12.4 item.index, reaching across to a sibling array.
    sum_ab = a.map(x) with (x + b[x.index]);
    // Result element type narrows int -> bit.
    gt = a.map(x) with (x > 1);
    // Queue receiver yields a queue.
    qd = q.map(x) with (x * 2);
    // Fixed receiver yields a same-range fixed array.
    fad = fa.map(x) with (x + 1);
    // Empty receiver yields an empty result.
    em = empty.map(x) with (x + 1);
    // The element is itself an array; the closure reduces each row.
    rowsum = m.map(row) with (row.sum());
  end
endmodule
