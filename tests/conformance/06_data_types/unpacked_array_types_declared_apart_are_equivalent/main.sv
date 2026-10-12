// Two fixed-size unpacked array types are equivalent when their element types
// are equivalent and their sizes equal, whatever bounds each declares and
// wherever each is declared (LRM 6.22.2), and a structure type declared in a
// package matches itself in every scope it is imported into (LRM 6.22.1). So
// an array a package names by a typedef, one a module declares outright, and
// one another module declares with other bounds are assigned to one another
// element by element in left-to-right order (LRM 7.6), compared as wholes (LRM
// 7.4.6), connected through ports, and passed to one subroutine by value and
// by reference, which asks for equivalent types (LRM 13.5.2). That holds of an
// array whose elements are structures and of one of many elements alike.
module Holder(input int in[1:4], output int out[4]);
  import row_pkg::*;

  int held[3:0];
  int many[1:600];
  labelled_t labelled[1:2];

  assign out = in;

  task automatic bump(ref int row[4]);
    row[3] += 1;
  endtask
endmodule

module Top;
  import row_pkg::*;

  row_t mine;
  int plain[4];
  int through_port[0:3];
  int many[600];
  labelled_row_t labelled;
  int total;
  int total_of_holders;
  bit equal_across;
  bit equal_after_a_write;
  bit many_equal_across;
  bit labelled_equal_across;

  Holder holder(.in(mine), .out(through_port));

  task automatic take(ref row_t row, input row_t given);
    row = given;
  endtask

  initial begin
    mine = '{1, 2, 3, 4};
    holder.held = mine;
    plain = holder.held;
    equal_across = (holder.held == mine) && (plain == holder.held);
    holder.held[0] = 40;
    equal_after_a_write = 1'b1;
    equal_after_a_write = (holder.held == mine);

    total = sum_of(plain);
    total_of_holders = sum_of(holder.held);
    take(holder.held, '{5, 6, 7, 8});
    holder.bump(mine);

    foreach (many[i]) many[i] = i;
    holder.many = many;
    many_equal_across = (holder.many == many);
    holder.many[600] = -1;
    many[0] = -2;
    many = holder.many;

    labelled[1] = '{tag: 7, name: "seven"};
    holder.labelled = labelled;
    labelled_equal_across = (holder.labelled == labelled);
    holder.labelled[1].name = "one";
    labelled = holder.labelled;
    #1;
  end

  final begin
    if (plain[0] !== 1 || plain[3] !== 4)
      $fatal(1, "an array assigned from another module's was %p", plain);
    if (equal_across !== 1'b1)
      $fatal(1, "arrays of equivalent types declared apart compared unequal");
    if (equal_after_a_write !== 1'b0)
      $fatal(1, "arrays differing in one element compared equal");
    if (total !== 10)
      $fatal(1, "a module's array summed to %0d, expected 10", total);
    if (total_of_holders !== 46)
      $fatal(1, "another module's array summed to %0d, expected 46",
             total_of_holders);
    if (holder.held[3] !== 5 || holder.held[2] !== 6 || holder.held[1] !== 7 ||
        holder.held[0] !== 8)
      $fatal(1, "another module's array written through a reference was %p",
             holder.held);
    if (mine[0] !== 1 || mine[3] !== 5)
      $fatal(1, "an array another module's task wrote through was %p", mine);
    if (through_port[0] !== 1 || through_port[3] !== 5)
      $fatal(1, "an array carried through two ports was %p", through_port);
    if (many_equal_across !== 1'b1)
      $fatal(1, "large arrays of equivalent types compared unequal");
    if (many[0] !== 0 || many[599] !== -1 || many[300] !== 300)
      $fatal(1, "a large array assigned back held %0d %0d %0d", many[0],
             many[599], many[300]);
    if (labelled_equal_across !== 1'b1)
      $fatal(1, "arrays of one package's structure compared unequal");
    if (labelled[0].name != "one" || labelled[1].name != "seven" ||
        labelled[1].tag !== 7)
      $fatal(1, "an array of structures assigned back was %p", labelled);
    $display("All checks passed");
  end
endmodule
