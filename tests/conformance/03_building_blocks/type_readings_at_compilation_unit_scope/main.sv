// The compilation-unit scope holds any item a package may hold and has no
// instance either (LRM 3.12.1, 26.2), so a subroutine declared there reaches
// the readings a type's own declaration decides for the same reason a package
// subroutine does: an enumeration's declared name and its neighbour in the
// declared order (LRM 6.19.5), and the assignment pattern %p prints an
// aggregate as (LRM 21.2.1.6).
typedef enum {IDLE, RUN, STOP} unit_state_t;
typedef struct {
  int count;
  unit_state_t state;
} unit_entry_t;

function automatic string unit_name_of(unit_state_t s);
  return s.name();
endfunction

function automatic unit_state_t unit_after(unit_state_t s);
  return s.next();
endfunction

function automatic string unit_pattern_of(unit_entry_t e);
  return $sformatf("%p", e);
endfunction

module Top;
  string run_name;
  unit_state_t next_state;
  string entry_text;

  initial begin
    run_name = unit_name_of(RUN);
    next_state = unit_after(RUN);
    entry_text = unit_pattern_of('{count: 4, state: IDLE});
  end

  final begin
    if (run_name !== "RUN") $fatal(1, "run_name was '%s', expected RUN", run_name);
    if (next_state !== STOP)
      $fatal(1, "unit_after(RUN) was '%s', expected STOP", next_state.name());
    if (entry_text !== "'{count:4, state:IDLE}")
      $fatal(1, "entry_text was %s, expected '{count:4, state:IDLE}",
             entry_text);
    $display("All checks passed");
  end
endmodule
