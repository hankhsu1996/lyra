// An interface class reached along two paths is one type (LRM 8.26.6.3), and
// one implementation answers every prototype of that name the class is
// committed to (LRM 8.26.6.1). That holds wherever each class is declared: the
// root of the diamond in one package, its two sides in a second and a third,
// the interface class joining them in a fourth, and the classes implementing
// it -- one answering everything itself, one inheriting the answers from a
// base of another package -- in a module. A handle of each interface class of
// the diamond reaches the same body, and a cast between them succeeds on an
// object of a class implementing the join (LRM 8.26.5, 26.3).
package root_pkg;
  interface class Root;
    pure virtual function int id();
  endclass
endpackage

package left_pkg;
  interface class Left extends root_pkg::Root;
    pure virtual function int left();
  endclass
endpackage

package right_pkg;
  interface class Right extends root_pkg::Root;
    pure virtual function int right();
  endclass
endpackage

package join_pkg;
  interface class Both extends left_pkg::Left, right_pkg::Right;
  endclass

  class Answers;
    virtual function int id();
      return 5;
    endfunction

    virtual function int left();
      return 6;
    endfunction

    virtual function int right();
      return 7;
    endfunction
  endclass
endpackage

module Top;
  class Own implements join_pkg::Both;
    virtual function int id();
      return 1;
    endfunction

    virtual function int left();
      return 2;
    endfunction

    virtual function int right();
      return 3;
    endfunction
  endclass

  // Every answer is inherited from a class of another package.
  class Inherited extends join_pkg::Answers implements join_pkg::Both;
  endclass

  int own_id_via_root;
  int own_id_via_left;
  int own_id_via_right;
  int own_id_via_both;
  int own_left;
  int own_right;
  int inherited_id_via_root;
  int inherited_id_via_left;
  int inherited_id_via_right;
  int inherited_left;
  int inherited_right;
  bit left_to_right_cast;
  int right_after_cast;

  initial begin
    Own own;
    Inherited inherited;
    root_pkg::Root root_ref;
    left_pkg::Left left_ref;
    right_pkg::Right right_ref;
    join_pkg::Both both_ref;

    own = new;
    root_ref = own;
    own_id_via_root = root_ref.id();
    left_ref = own;
    own_id_via_left = left_ref.id();
    own_left = left_ref.left();
    right_ref = own;
    own_id_via_right = right_ref.id();
    own_right = right_ref.right();
    both_ref = own;
    own_id_via_both = both_ref.id();

    inherited = new;
    root_ref = inherited;
    inherited_id_via_root = root_ref.id();
    left_ref = inherited;
    inherited_id_via_left = left_ref.id();
    inherited_left = left_ref.left();
    right_ref = inherited;
    inherited_id_via_right = right_ref.id();
    inherited_right = right_ref.right();

    left_ref = inherited;
    left_to_right_cast = $cast(right_ref, left_ref);
    right_after_cast = right_ref.right();
  end

  final begin
    if (own_id_via_root !== 1)
      $fatal(1, "own_id_via_root was %0d, expected 1", own_id_via_root);
    if (own_id_via_left !== 1)
      $fatal(1, "own_id_via_left was %0d, expected 1", own_id_via_left);
    if (own_id_via_right !== 1)
      $fatal(1, "own_id_via_right was %0d, expected 1", own_id_via_right);
    if (own_id_via_both !== 1)
      $fatal(1, "own_id_via_both was %0d, expected 1", own_id_via_both);
    if (own_left !== 2) $fatal(1, "own_left was %0d, expected 2", own_left);
    if (own_right !== 3)
      $fatal(1, "own_right was %0d, expected 3", own_right);
    if (inherited_id_via_root !== 5)
      $fatal(1, "inherited_id_via_root was %0d, expected 5",
             inherited_id_via_root);
    if (inherited_id_via_left !== 5)
      $fatal(1, "inherited_id_via_left was %0d, expected 5",
             inherited_id_via_left);
    if (inherited_id_via_right !== 5)
      $fatal(1, "inherited_id_via_right was %0d, expected 5",
             inherited_id_via_right);
    if (inherited_left !== 6)
      $fatal(1, "inherited_left was %0d, expected 6", inherited_left);
    if (inherited_right !== 7)
      $fatal(1, "inherited_right was %0d, expected 7", inherited_right);
    if (left_to_right_cast !== 1)
      $fatal(1, "left_to_right_cast was %0d, expected 1", left_to_right_cast);
    if (right_after_cast !== 7)
      $fatal(1, "right_after_cast was %0d, expected 7", right_after_cast);
    $display("All checks passed");
  end
endmodule
