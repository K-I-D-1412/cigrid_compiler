/*
 * Liveness Analysis Test Case
 * 
 * This program tests various aspects of liveness analysis:
 * 1. Variable initialization (def without prior use)
 * 2. Sequential flow (straight-line code)
 * 3. Conditional branches (if-else with different variable usage)
 * 4. Loops (while loop with back-edges)
 * 5. Dead variables (variables defined but not used)
 * 6. Variable live ranges across control flow
 * 
 */

int main() {
    // ========== Test 1: Simple Sequential Flow ==========
    // Expected: a defined here, should be live until first use
    int a = 5;        // def={vreg1}, use={}, live-out={vreg1}
    
    // Expected: b defined here, should be live until used in condition
    int b = 10;       // def={vreg2}, use={}, live-out={vreg1, vreg2}
    
    // Expected: c defined here, initially dead (value will be overwritten)
    int c = 0;        // def={vreg3}, use={}, live-out={vreg1, vreg2, vreg3}
    
    
    // ========== Test 2: Conditional Branch ==========
    // Expected: vreg1 (a) is live-in, vreg2 (b) becomes live-out on both branches
    if (a < b) {      // use={vreg1, vreg2}
        // True branch: c = a + b
        // Expected: vreg1, vreg2 live-in; vreg3 def
        c = a + b;    // def={vreg3}, use={vreg1, vreg2}, live-out={vreg1, vreg3}
    } else {
        // False branch: c = a - b
        // Expected: vreg1, vreg2 live-in; vreg3 def
        c = a - b;    // def={vreg3}, use={vreg1, vreg2}, live-out={vreg1, vreg3}
    }
    // After if-else: vreg3 (c) is definitely defined
    
    
    // ========== Test 3: Loop with Back-Edge ==========
    // Expected: d initialized, will be modified in loop
    int d = 0;        // def={vreg4}, use={}, live-out={vreg1, vreg3, vreg4}
    
    // Loop guard: a > 0
    // Expected: vreg1 live-in (checked in condition)
    // Expected: back-edge from loop body causes vreg1, vreg3, vreg4 to be live
    while (a > 0) {   // use={vreg1}, live-in={vreg1, vreg3, vreg4}
        
        // Inside loop: d = d + c
        // Expected: vreg3 (c), vreg4 (d) both live-in
        // Expected: vreg4 redefined
        d = d + c;    // def={vreg4}, use={vreg3, vreg4}, live-in={vreg1, vreg3, vreg4}
        
        // Expected: vreg1 (a) decremented, redefined
        a = a - 1;    // def={vreg1}, use={vreg1}, live-in={vreg1, vreg3, vreg4}
        
        // Back-edge: jumps to while guard
        // Expected: vreg1, vreg3, vreg4 all live-out (needed in next iteration)
    }
    // After loop exit: vreg1 (a) is dead (no longer > 0)
    
    
    // ========== Test 4: Dead Code (Variable Defined but Not Used) ==========
    // Expected: vreg2 (b) was defined earlier but never used after this point
    // This tests if liveness correctly identifies dead variables
    b = 99;           // def={vreg2}, use={}, live-out={vreg3, vreg4}
    // vreg2 (b) becomes immediately dead here (never read after this)
    
    
    // ========== Test 5: Final Computation ==========
    // Expected: result depends on c and d
    int result = c + d;  // def={vreg5}, use={vreg3, vreg4}, live-out={vreg5}
    
    
    // ========== Test 6: Return Statement ==========
    // Expected: only vreg5 (result) is live-in to return
    return result;    // use={vreg5}, live-in={vreg5}
}