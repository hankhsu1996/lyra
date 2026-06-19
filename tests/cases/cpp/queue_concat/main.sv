// LRM 10.10 unpacked array concatenation into a queue, including the push /
// pop / insert idioms of LRM 7.10.4. An operand that is a queue is spliced in
// element order; any other operand is a single element. The empty `{}` is the
// empty queue, and a concat reseeds the element default so a later append still
// carries the element shape.
module Top;
  int pb [$] = '{1, 2, 3};
  int pf [$] = '{1, 2, 3};
  int ins [$] = '{1, 2, 3, 4};
  int a [$] = '{10, 20};
  int b [$] = '{30, 40};
  int cat [$];
  int emptied [$] = '{7, 8};
  int reseed [$] = '{7, 8};

  initial begin
    pb = {pb, 4};
    pf = {0, pf};
    ins = {ins[0:1], 99, ins[2:$]};
    cat = {a, b};
    emptied = {};
    reseed = {};
    reseed = {reseed, 5};
  end
endmodule
