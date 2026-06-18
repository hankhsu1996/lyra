module Top;
  int m [string];
  int n [int];

  int va;
  int vb;
  int ex_a;
  int ex_z;
  int sz1;
  int vmiss;
  int sz2;
  int vc;
  int sz3;
  int sz4;
  int ex_b;

  int nv10;
  int nv3;
  int nsz;
  int nmiss;
  int nsz2;

  // A method on a nested receiver (`mm[i]`) must reach the stored sub-map: a
  // const query reads it and `delete` mutates it in place.
  int mm [string][int];
  int nest_ex;
  int nest_after_delete;

  initial begin
    m["a"] = 1;
    m["b"] = 2;
    va = m["a"];
    vb = m["b"];
    ex_a = m.exists("a");
    ex_z = m.exists("z");
    sz1 = m.num();

    // LRM 7.8.6: reading a nonexistent entry returns the element default and
    // does not allocate; sz2 proves num() is unchanged by the read.
    vmiss = m["z"];
    sz2 = m.num();

    // LRM 7.8.7: a compound write to a nonexistent entry allocates the element
    // default (0) first, then applies the operation.
    m["c"] += 5;
    vc = m["c"];
    sz3 = m.num();

    m.delete("b");
    sz4 = m.num();
    ex_b = m.exists("b");

    n[10] = 100;
    n[3] = 30;
    nv10 = n[10];
    nv3 = n[3];
    nsz = n.num();
    nmiss = n[7];
    nsz2 = n.num();

    mm["x"][1] = 10;
    mm["x"][2] = 20;
    nest_ex = mm["x"].exists(2);
    mm["x"].delete(1);
    nest_after_delete = mm["x"].num();
  end
endmodule
