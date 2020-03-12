;; translate s-expr to XML optimized for AIML 
;;
(define (expr2xml expr (level 0))
 (cond 
   ((or (atom? expr) (quote? expr))
       (print (dup "  " level))
       (if last-expr-was-pattern
        (and (print expr "  ") (setq last-expr-was-pattern nil))
        (println expr)))
   ((list? (first expr))
       (expr2xml (first expr) (+ level 1))
       (dolist (s (rest expr)) (expr2xml s (+ level 1))))
   ((symbol? (first expr))
       (print (dup "  " level))
       (if (= (first expr) 'pattern)
        (and (print "<" (first expr) ">") (setq last-expr-was-pattern true))	
        (println "<" (first expr) ">"))
       (dolist (s (rest expr)) (expr2xml s (+ level 1)))
       (print (dup "  " level))
       (println "</" (first expr) ">"))
   (true
      (print (dup "  " level) 
      (println "<error>" (string expr) "<error>")))
 ))
