; http://community.schemewiki.org/cgi-bin/scheme.cgi?comment-style
; a procedure that modifies something 
 (define (look mutator) 
   ...) 
  
 (define (factorial n) 
   (if (= n 0)                           ; This could also be (< N 2). 
       1                                 ; Base case 
       (* n (factorial (- n 1)))))       ; Recursive case 
