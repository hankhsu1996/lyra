// A hierarchical name reaches a variable holding a handle (LRM 23.8), and a
// method entered through that handle is resolved the way any other call on a
// handle is: a non-virtual method runs what the class the access names declares
// (LRM 8.14), and a virtual one runs what the object's own class answers with
// (LRM 8.20, 8.22). The reader outside the declaring scope has no name for that
// class (LRM 23.9), and neither rule depends on one -- so the same handle must
// answer 4 and 2 to two calls written side by side, which is what separates the
// two rules from any single answer that happens to satisfy one of them.
//
// A method that takes an argument and one that changes the object are the same
// mechanism, and they are here because an entry reached this way has to carry
// what the source wrote and land its effect on the object the handle holds.
module Child;
  class Animal;
    int legs = 4;
    function int Count();
      return legs;
    endfunction
    virtual function int Speak();
      return 1;
    endfunction
    function int Plus(int n);
      return legs + n;
    endfunction
    virtual function void Lose();
      legs = legs - 1;
    endfunction
  endclass

  class Bird extends Animal;
    function int Count();
      return 2;
    endfunction
    virtual function int Speak();
      return 2;
    endfunction
    virtual function void Lose();
      legs = legs - 2;
    endfunction
  endclass

  Animal pet;

  int up_count = 0;
  int up_speak = 0;
  int up_plus = 0;
  int up_legs_after = 0;

  initial begin
    Bird b = new();
    pet = b;
  end

  initial begin
    #2;
    up_count = Top.pet.Count();
    up_speak = Top.pet.Speak();
    up_plus = Top.pet.Plus(10);
    Top.pet.Lose();
    up_legs_after = Top.pet.Count();
  end
endmodule

module Top;
  class Shape;
    int sides = 4;
    function int Count();
      return sides;
    endfunction
    virtual function int Speak();
      return 1;
    endfunction
    function int Plus(int n);
      return sides + n;
    endfunction
    virtual function void Lose();
      sides = sides - 1;
    endfunction
  endclass

  class Triangle extends Shape;
    function int Count();
      return 2;
    endfunction
    virtual function int Speak();
      return 2;
    endfunction
    virtual function void Lose();
      sides = sides - 2;
    endfunction
  endclass

  Shape pet;

  Child kid ();

  int down_count = 0;
  int down_speak = 0;
  int down_plus = 0;
  int down_legs_after = 0;

  initial begin
    Triangle t = new();
    pet = t;
  end

  initial begin
    #2;
    down_count = kid.pet.Count();
    down_speak = kid.pet.Speak();
    down_plus = kid.pet.Plus(10);
    kid.pet.Lose();
    down_legs_after = kid.pet.Count();
  end

  final begin
    if (down_count !== 4)
      $fatal(1, "a downward non-virtual call answered %0d, expected 4", down_count);
    if (down_speak !== 2)
      $fatal(1, "a downward virtual call answered %0d, expected 2", down_speak);
    if (down_plus !== 14)
      $fatal(1, "a downward call with an argument answered %0d, expected 14", down_plus);
    if (down_legs_after !== 2)
      $fatal(1, "a downward call left the object at %0d, expected 2", down_legs_after);
    if (kid.up_count !== 4)
      $fatal(1, "an upward non-virtual call answered %0d, expected 4", kid.up_count);
    if (kid.up_speak !== 2)
      $fatal(1, "an upward virtual call answered %0d, expected 2", kid.up_speak);
    if (kid.up_plus !== 14)
      $fatal(1, "an upward call with an argument answered %0d, expected 14", kid.up_plus);
    if (kid.up_legs_after !== 2)
      $fatal(1, "an upward call left the object at %0d, expected 2", kid.up_legs_after);
    $display("All checks passed");
  end
endmodule
