module Top;
  int q [$] = '{1, 2, 3};
  int sz;
  int pf;
  int pb;

  // pop on an empty queue returns the element default (LRM 7.10.2.4 / 7.10.2.5).
  int eq [$];
  int pe;

  // delete() with no index clears the queue (LRM 7.10.2.3).
  int cq [$] = '{7, 8};

  // An invalid insert index is a no-op (LRM 7.10.2.2).
  int badq [$] = '{1, 2};

  initial begin
    q.push_back(4);
    q.push_front(0);
    sz = q.size();
    pf = q.pop_front();
    pb = q.pop_back();
    q.insert(1, 9);
    q.delete(2);

    pe = eq.pop_front();

    cq.delete();

    badq.insert(99, 5);
  end
endmodule
