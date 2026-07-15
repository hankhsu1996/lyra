// LRM 20.7 array query functions over a dynamically sized dimension. The
// dimension's direction is a property of its kind, not of its contents: a queue
// or dynamic array dimension ascends from 0, so `$left` and `$low` are 0 and
// `$increment` is -1 whatever the array holds. Only the extent depends on the
// current state, so `$size` reads it and `$right` / `$high` are the last
// position -- which is -1 while the dimension is empty.
//
// An associative dimension's index space belongs to its index type instead: it
// ascends from 0 to that type's highest value. What the array holds decides only
// how many indices are allocated and which are the smallest and largest, so
// `$low` / `$high` report those and read the index type's default -- `'x` for a
// 4-state index type -- while none is allocated.
module Top;
  int    dyn[];
  int    que[$];
  string str;

  int    map[int];
  int    map4[logic [7:0]];

  int dyn_size, dyn_left, dyn_right, dyn_low, dyn_high, dyn_incr;
  int que_size, que_right, que_high;
  int str_size, str_left, str_right;
  int empty_size, empty_right, empty_high;

  int map_size, map_left, map_right, map_low, map_high, map_incr;
  logic [7:0] map4_low, map4_high, map4_right;
  logic [7:0] unallocated_low, unallocated_high;

  // LRM 20.7 with a run-time dimension index over an array whose top dimension
  // is dynamic. `int [][5]` has three dimensions -- the dynamic outer, the
  // fixed unpacked `[5]`, and the element `int`'s packed `[31:0]`. The top
  // dimension's extent is read from the value; the deeper fixed dimensions keep
  // their constant shape; an index the type has no dimension for reads `'x`.
  int rt_top[][5];
  int rt_dim;
  integer rt_top1, rt_top2, rt_top3, rt_top_oor;

  initial begin
    dyn = new[4];
    que.push_back(10);
    que.push_back(20);
    que.push_back(30);
    str = "hello";

    dyn_size = $size(dyn);
    dyn_left = $left(dyn);
    dyn_right = $right(dyn);
    dyn_low = $low(dyn);
    dyn_high = $high(dyn);
    dyn_incr = $increment(dyn);

    que_size = $size(que);
    que_right = $right(que);
    que_high = $high(que);

    str_size = $size(str);
    str_left = $left(str);
    str_right = $right(str);

    // A negative index sorts below a positive one: the index type is signed.
    map[10] = 1;
    map[-3] = 2;
    map[7] = 3;
    map_size = $size(map);
    map_left = $left(map);
    map_right = $right(map);
    map_low = $low(map);
    map_high = $high(map);
    map_incr = $increment(map);

    map4[8'hA0] = 1;
    map4[8'h0F] = 2;
    map4_low = $low(map4);
    map4_high = $high(map4);
    map4_right = $right(map4);

    // Nothing allocated: the index reported is the index type's default.
    map4.delete();
    unallocated_low = $low(map4);
    unallocated_high = $high(map4);

    que.delete();
    empty_size = $size(que);
    empty_right = $right(que);
    empty_high = $high(que);

    rt_top = new[3];
    rt_dim = 1;
    rt_top1 = $size(rt_top, rt_dim);
    rt_dim = 2;
    rt_top2 = $size(rt_top, rt_dim);
    rt_dim = 3;
    rt_top3 = $size(rt_top, rt_dim);
    rt_dim = 4;
    rt_top_oor = $size(rt_top, rt_dim);
  end
endmodule
