// A class body may declare a method as an extern prototype and leave the body
// outside it, the two tied together by qualifying the method name with the
// class name. The prototype carries the qualifiers and the specifiers; the
// out-of-block declaration drops them and otherwise matches exactly, except
// that it may leave out a default argument value the prototype gave and that it
// must reach for the class scope to name a return type the class declares. The
// split is a matter of where the text sits and of nothing else: the body sees
// every declaration of the class as an inline one would, a prototype declared
// virtual still dispatches to the override a derived class defines the same
// way, a static one still has no receiver, and a protected one is still
// reachable from within the class. A prototype may be a task as readily as a
// function (LRM 8.18, 8.20, 8.24, 13.3).
module Top;
  timeunit 1ns;
  timeprecision 1ps;

  class Ledger;
    typedef int Amount;

    int balance;

    extern function int deposit(input int amount = 5);
    extern function Amount doubled(input int value);
    extern virtual function int rate();
    extern task settle(input int amount, output int closing);
    extern static function int fee(input int amount);
    extern protected function int guarded();

    function int visible();
      return guarded();
    endfunction
  endclass

  function int Ledger::deposit(input int amount);
    balance = balance + amount;
    return balance;
  endfunction

  function Ledger::Amount Ledger::doubled(input int value);
    return value * 2;
  endfunction

  function int Ledger::rate();
    return 5;
  endfunction

  task Ledger::settle(input int amount, output int closing);
    #3;
    balance = balance - amount;
    closing = balance;
  endtask

  function int Ledger::fee(input int amount);
    return amount / 10;
  endfunction

  function int Ledger::guarded();
    return 77;
  endfunction

  class Premium extends Ledger;
    extern virtual function int rate();
  endclass

  function int Premium::rate();
    return 9;
  endfunction

  Ledger l;
  Premium p;
  Ledger base_handle;

  int after_default;
  int after_deposit;
  int doubled_value;
  int closing;
  int settle_time;
  int base_rate;
  int derived_rate;
  int charged;
  int guarded_value;

  initial begin
    after_default = -1;
    after_deposit = -1;
    doubled_value = -1;
    closing = -1;
    settle_time = -1;
    base_rate = -1;
    derived_rate = -1;
    charged = -1;
    guarded_value = -1;

    l = new;
    after_default = l.deposit();
    after_deposit = l.deposit(100);
    doubled_value = l.doubled(21);

    l.settle(30, closing);
    settle_time = $time;

    base_rate = l.rate();
    p = new;
    base_handle = p;
    derived_rate = base_handle.rate();

    charged = Ledger::fee(250);
    guarded_value = l.visible();
  end

  final begin
    if (after_default !== 5)
      $fatal(1, "after_default was %0d, expected 5", after_default);
    if (after_deposit !== 105)
      $fatal(1, "after_deposit was %0d, expected 105", after_deposit);
    if (doubled_value !== 42)
      $fatal(1, "doubled_value was %0d, expected 42", doubled_value);
    if (closing !== 75) $fatal(1, "closing was %0d, expected 75", closing);
    if (settle_time !== 3)
      $fatal(1, "settle_time was %0d, expected 3", settle_time);
    if (l.balance !== 75)
      $fatal(1, "l.balance was %0d, expected 75", l.balance);
    if (base_rate !== 5) $fatal(1, "base_rate was %0d, expected 5", base_rate);
    if (derived_rate !== 9)
      $fatal(1, "derived_rate was %0d, expected 9", derived_rate);
    if (charged !== 25) $fatal(1, "charged was %0d, expected 25", charged);
    if (guarded_value !== 77)
      $fatal(1, "guarded_value was %0d, expected 77", guarded_value);
    $display("All checks passed");
  end
endmodule
