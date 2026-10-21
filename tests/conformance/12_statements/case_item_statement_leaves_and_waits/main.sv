// The statement a case item runs is any statement (LRM 12.5): one that leaves
// the loop or the subroutine the case stands in, or one that waits. Each takes
// effect from inside the item exactly as it would outside a case, whether the
// labels are plain values, carry don't-care bits (LRM 12.5.1), or are computed
// while the program runs.
module Top;
  int step;

  function automatic int first_odd(int limit);
    for (int i = 0; i < limit; i++) begin
      case (i % 2)
        0: ;
        1: return i;
      endcase
    end
    return -1;
  endfunction

  function automatic int first_odd_by_bit(int limit);
    for (int i = 0; i < limit; i++) begin
      casez (i[1:0])
        2'b?0: ;
        2'b?1: return i;
      endcase
    end
    return -1;
  endfunction

  int visited, skipped, wild_visited, wild_skipped, computed_visited, waited_at;

  initial begin
    visited = 0;
    skipped = 0;
    for (int i = 0; i < 10; i++) begin
      case (i)
        2, 4: begin
          skipped++;
          continue;
        end
        6: break;
        default: ;
      endcase
      visited++;
    end

    wild_visited = 0;
    wild_skipped = 0;
    for (int i = 0; i < 16; i++) begin
      casez (i[3:0])
        4'b??10: begin
          wild_skipped++;
          continue;
        end
        4'b1???: break;
        default: ;
      endcase
      wild_visited++;
    end

    step = 3;
    computed_visited = 0;
    for (int i = 0; i < 20; i++) begin
      case (i)
        step * 2: continue;
        step * 3: break;
        default: ;
      endcase
      computed_visited++;
    end

    case (visited)
      4: begin
        #3;
        waited_at = $time;
      end
      default: waited_at = -1;
    endcase

    if (visited !== 4) $fatal(1, "the loop ran its rest %0d times", visited);
    if (skipped !== 2) $fatal(1, "the loop was continued %0d times", skipped);
    if (wild_visited !== 6 || wild_skipped !== 2)
      $fatal(1, "the casez loop ran %0d and continued %0d times", wild_visited,
             wild_skipped);
    if (computed_visited !== 8)
      $fatal(1, "the loop over computed labels ran %0d times", computed_visited);
    if (first_odd(9) !== 1) $fatal(1, "the return left at %0d", first_odd(9));
    if (first_odd_by_bit(9) !== 1)
      $fatal(1, "the casez return left at %0d", first_odd_by_bit(9));
    if (waited_at !== 3) $fatal(1, "the item waited until %0d", waited_at);
    $display("All checks passed");
  end
endmodule
