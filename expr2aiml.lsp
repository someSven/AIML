(setq responses '())
(load (append ((exec "echo $HOME")0) "/AIML/responses.lsp"))
(setq save-path (append ((exec "echo $HOME")0) "/AIML/"))
(setq export-filename "exported")
(setq export-file (append ((exec "echo $HOME")0) "/AIML/" export-filename ".aiml"))
(setq export-file-header {<aiml version="1.0.1" encoding="UTF-8">
<!-- basic_chat.aiml -->})
(load "expr2append-xml.lsp")

;;backup
(setq last-backup (- (date-value) (integer (first(parse (first(sort (exec (append "ls " save-path "/backups"))>))".")))))
(if (> last-backup 1000) (save (append save-path "backups/" (string(date-value)) ".lsp") 'responses))
; add only save if new, deleted old files?

;; translate s-expr to AIML XML
;; forked from http://www.newlisp.org/index.cgi?page=S-expressions_to_XML
;;
(define (expr2xml expr (level 0) (indentation 4))
 (setq indentation (dup " " indentation))
 (cond 
   ((or (atom? expr) (quote? expr))
       (print (dup indentation level))
       (if last-expr-was-pattern
        (and (print expr (dup indentation level)) (setq last-expr-was-pattern nil))
        (println expr)))
   ((list? (first expr))		
       (expr2xml (first expr) (+ level 1))
       (dolist (s (rest expr)) (expr2xml s (+ level 1))))
   ((symbol? (first expr))
       (print (dup indentation level))
       (if (= (first expr) 'pattern)
        (and (print "<" (first expr) ">") (setq last-expr-was-pattern true))
	(println "<" (first expr) ">"))
       (if (= (first expr) 'random)
        (dolist (s (map (fn (el) (append (append "<li>" el)"</li>")) (rest expr))) (expr2xml s (+ level 1)))
        (dolist (s (rest expr)) (expr2xml s (+ level 1))))
       (print (dup indentation level))
       (println "</" (first expr) ">"))
   (true
      (print (dup indentation level) 
      (println "<error>" (string expr) "<error>")))
 ))

;; functions to create new responses, update or alter them
;;
(define (help)
	(println "--------------------(help) for this overview------------------------------------") 
	(println "commands: set-newrp, update-rp, save-data, response-exists?, newrp, rp2xml, expr2xml")
	(println "save-path: " save-path "\t" "export-file: " export-file)
	(println "--------------------------------------------------------------------------------") true)

(define (set-newrp pattern-input template-input)
	(setq pattern-input (upper-case pattern-input))
	(push (list 'category (cons 'pattern pattern-input) (cons 'template template-input)) responses -1)
	(last responses))

(define (update-rp pattern-input template-input)
	(setq pattern-input (upper-case pattern-input))
	(setq counter -1) 
	(dolist (entry responses) 
		(inc counter) (if (= (lookup 'pattern entry) pattern-input) 
		(setf (assoc 'template (responses counter)) (cons 'template template-input)) ))
	(last responses))

(define (save-data) (save (append save-path "responses.lsp") 'responses))

;(define (export) (push "\n" export-data -1) (push export-data {</aiml>} -1) (write-file export-file export-data))
(define (export) 
	(write-file export-file export-file-header)
	(expr2append-xml (responses 2) 1)
	(append-file export-file {</aiml>}))

(define (response-exists? pattern-input)
	(setq pattern-input (upper-case pattern-input)) 
	(if (dolist (entry responses) (= (lookup 'pattern entry) pattern-input))))

(define (newrp pattern-input template-input)
	(setq pattern-input (upper-case pattern-input)) 
	(if (catch(response-exists? pattern-input)) (throw-error "Pattern already exists!") (set-newrp pattern-input template-input))) 

(define (rp2xml pattern-input template-input)
	(setq pattern-input (upper-case pattern-input)) 
	(expr2xml (list 'category (cons 'pattern pattern-input) (cons 'template template-input))))  

; (define (save-exit) (save save-data) (exit))

(help)
;; ToDo List
;; Hinzufügen, Reset Responses, Kategorien zusammenfassen, Random 










