// An input port written with no port kind is a net of the default net type
// (LRM 23.2.2.3), and a net's data type shall be a 4-state integral type or an
// aggregate of them (LRM 6.7.1), so a port of any other type written that way
// is forbidden text. The front end accepts it as a net of the type written,
// whose meaning the standard does not give; Lyra gives it the nearest allowed
// text's. The net types' truth tables are over bits, so a 2-state integral net
// folds its drivers as a wire does and takes an x or z the fold makes as 0
// (LRM 6.11.2): two drivers giving 1 and 0 make 0, and nothing driving it
// reads 0. A structure folds member by member where every member can hold the
// z a driver leaves where it drives nothing, so a driver of one member leaves
// the others to the rest. A real, a string, a class handle, or a structure
// holding one of those or a 2-state member has no fold, which makes it a net
// of a user-defined nettype with no resolution function (LRM 6.6.7): its one
// driver decides it, where nothing drives it it holds its type's default, and
// the net type it is declared with contributes nothing, as a nettype has no
// contribution of its own; a `tri1` still pulls a 2-state integral net to 1.
class Item;
  int tag;
endclass

typedef struct {
  int count;
  logic [3:0] flags;
} counted_t;

typedef struct {
  string label;
  int weight;
} labelled_t;

typedef struct {
  logic [7:0] data;
  logic [3:0] mask;
} pins_t;

module Leaf (
    input int whole,
    input byte narrow,
    input bit [99:0] wide,
    input real level,
    input shortreal fine,
    input string name,
    input Item item,
    input counted_t counted,
    input labelled_t labelled
);
endmodule

`default_nettype tri1
module Pulled (
    input int whole,
    input real level,
    input labelled_t labelled
);
endmodule
`default_nettype wire

module Contested (
    input int both,
    input int agree,
    input pins_t members
);
  assign both = 32'h0000_00ff;
  assign agree = 32'h0000_000f;
  assign members.mask = 4'b1100;
endmodule

module Top;
  int whole = 7;
  byte narrow = -3;
  bit [99:0] wide = {4'h9, 96'h1};
  real level = 2.5;
  shortreal fine = 1.25;
  string name = "seven";
  Item item = new;
  counted_t counted = '{count: 11, flags: 4'b10x1};
  labelled_t labelled = '{label: "left", weight: 4};

  Leaf driven (.*);
  Leaf undriven ();
  Pulled pulled (.whole(whole), .level(level), .labelled(labelled));
  Pulled floating ();
  Contested contested (
      .both(32'h0000_0f0f),
      .agree(32'h0000_000f),
      .members('{data: 8'h0f, mask: 4'b1010})
  );

  initial begin
    item.tag = 5;
    #1;
    if (driven.whole !== 7) $fatal(1, "whole was %0d, expected 7", driven.whole);
    if (driven.narrow !== -3) $fatal(1, "narrow was %0d, expected -3", driven.narrow);
    if (driven.wide !== {4'h9, 96'h1}) $fatal(1, "wide was %h", driven.wide);
    if (driven.level != 2.5) $fatal(1, "level was %f, expected 2.5", driven.level);
    if (driven.fine != 1.25) $fatal(1, "fine was %f, expected 1.25", driven.fine);
    if (driven.name != "seven") $fatal(1, "name was '%s'", driven.name);
    if (driven.item != item) $fatal(1, "item was another object");
    if (driven.counted.count !== 11 || driven.counted.flags !== 4'b10x1)
      $fatal(1, "counted was %0d, %b", driven.counted.count, driven.counted.flags);
    if (driven.labelled.label != "left" || driven.labelled.weight !== 4)
      $fatal(1, "labelled was '%s', %0d", driven.labelled.label,
             driven.labelled.weight);

    whole = 9;
    name = "nine";
    #1;
    if (driven.whole !== 9) $fatal(1, "whole moved to %0d, expected 9", driven.whole);
    if (driven.name != "nine") $fatal(1, "name moved to '%s'", driven.name);

    if (undriven.whole !== 0) $fatal(1, "undriven whole was %0d", undriven.whole);
    if (undriven.narrow !== 0) $fatal(1, "undriven narrow was %0d", undriven.narrow);
    if (undriven.wide !== '0) $fatal(1, "undriven wide was %h", undriven.wide);
    if (undriven.level != 0.0) $fatal(1, "undriven level was %f", undriven.level);
    if (undriven.fine != 0.0) $fatal(1, "undriven fine was %f", undriven.fine);
    if (undriven.name != "") $fatal(1, "undriven name was '%s'", undriven.name);
    if (undriven.item != null) $fatal(1, "undriven item named an object");
    if (undriven.counted.count !== 0 || undriven.counted.flags !== 4'bxxxx)
      $fatal(1, "undriven counted was %0d, %b", undriven.counted.count,
             undriven.counted.flags);
    if (undriven.labelled.label != "" || undriven.labelled.weight !== 0)
      $fatal(1, "undriven labelled was '%s', %0d", undriven.labelled.label,
             undriven.labelled.weight);

    if (pulled.whole !== 9 || pulled.level != 2.5 || pulled.labelled.label != "left")
      $fatal(1, "pulled was %0d, %f, '%s'", pulled.whole, pulled.level,
             pulled.labelled.label);
    if (floating.whole !== -1) $fatal(1, "floating whole was %0d, expected -1", floating.whole);
    if (floating.level != 0.0) $fatal(1, "floating level was %f", floating.level);
    if (floating.labelled.label != "" || floating.labelled.weight !== 0)
      $fatal(1, "floating labelled was '%s', %0d", floating.labelled.label,
             floating.labelled.weight);

    // A force holds the net whatever its driver does, and a release shows the
    // driver again (LRM 10.6.2).
    force driven.level = 7.5;
    level = 3.5;
    #1;
    if (driven.level != 7.5) $fatal(1, "forced level was %f, expected 7.5", driven.level);
    release driven.level;
    #1;
    if (driven.level != 3.5) $fatal(1, "released level was %f, expected 3.5", driven.level);

    // 32'h0f0f against 32'h00ff: bits both drive 1 stay 1, bits both drive 0
    // stay 0, and bits they disagree on are x, read as 0.
    if (contested.both !== 32'h0000_000f)
      $fatal(1, "contested was %h, expected 0000000f", contested.both);
    if (contested.agree !== 32'h0000_000f)
      $fatal(1, "agreeing drivers gave %h, expected 0000000f", contested.agree);
    // The connection drives every member and the assignment only `mask`, so
    // `data` is the connection's and `mask` keeps the x where the two disagree.
    if (contested.members.data !== 8'h0f || contested.members.mask !== 4'b1xx0)
      $fatal(1, "contested members were %h, %b", contested.members.data,
             contested.members.mask);
    $display("All checks passed");
  end
endmodule
