(setq responses '())
(load (append ((exec "echo $HOME")0) "/AIML/responses.lsp"))
(setq save-path (append ((exec "echo $HOME")0) "/AIML/"))
(setq export-filename "exported")
(setq export-file (append ((exec "echo $HOME")0) "/AIML/" export-filename ".aiml"))
(setq export-file-header {<aiml version="1.0.1" encoding="UTF-8">
<!-- exported.aiml -->})
(load "expr2append-xml.lsp")
(setq indentation 4)
(setq indentation (dup " " indentation))

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

;; functions to create new aiml responses, update or alter them
;;
(define (help)
	(println "--------------------(help) for this overview------------------------------------") 
	(println "commands : [newrp] [update-rp] [save-data] [rp2xml] [expr2xml] [list2xml]")
	(println "commands : [set-topic] [random-list] [help] [reload] [export] [save-exit]")
	(println "--------------------------------------------------------------------------------")
	(println "debugging: [response-exists?] [response2that-exists?] [response-replace]")
	(println "debugging: [debuging only!: set-newrp]") 
	(println "save-path: " save-path "\t" "export-file: " export-file)
	(println "--------------------------------------------------------------------------------") true)

(define (reload) (! "clear") (load "expr2aiml.lsp") true)

(define (list2xml input) 
	(if	(catch (list? input)) 
		(and	(setq output "<random>\n")
			(dolist (element input)
				(push (string (dup indentation 4) "<li>" element "</li>\n") output -1))  
			(push (string (dup indentation 3) "</random>") output -1) output)
		(throw-error "Input in list2xml wasn't a list...")))

(define (set-newrp pattern-input template-input)
	(setq pattern-input (upper-case pattern-input))
	(if (string? template-input)
		(push (list 'category (cons 'pattern pattern-input) (cons 'template template-input)) responses -1)
		(push (list 'category (cons 'pattern pattern-input) (cons 'template (list template-input))) responses -1))
	(last responses))

(define (update-rp pattern-input template-input)
	(setq pattern-input (upper-case pattern-input))
	(setq counter -1) 
	(dolist (entry responses) 
		(inc counter) (if (= (lookup 'pattern entry) pattern-input) 
		(setf (assoc 'template (responses counter)) (cons 'template template-input)) ))
	(last responses))

(define (save-data) (save (append save-path "responses.lsp") 'responses))

(define (response-replace element) 
	(setq template (lookup 'template element))
	(setq entry (parse(first(parse (first(parse template " "))":"))"="))
	(setq leftover (join(rest(parse template ":"))":"))
	(if (first (= entry "gn")) 
		(setq varname (entry 1))
		(setq pattern {<get name=PLACEHOLDER/></get>}))
	(if (first (= entry "sn"))
		(setq varname (entry 1))
		(setq varvalue ((parse (entry 1) ",")1))
		(setq pattern {<set name=PLACEHOLDER/>VARVALUE</set>})
		(replace "VARVALUE" pattern varvalue))
	(replace "PLACEHOLDER" pattern varname)
	(setf (lookup 'template element) (string patter leftover))
	element)

(define (export) 
	(write-file export-file export-file-header)
	; (if (and topic (!= topic "") (!= "*"))  ) unnecessary?
	(append-file export-file "\n\n")
	(dolist (element responses)
		(replace "srai:" element {<srai>})
		(replace ":srai" element {</srai>})
		(replace "-sr/-" element {<sr/>})
                (replace "sr::" element {<srai>})
                (replace "::sr" element {</srai>})
		(replace "-star/-" element {<star/>})
		;(replace "that::" element {<that>}) ; before template
		;(replace "::that" element {</that>})
		(if (find "::" element) (response-replace element))
		(if (list? ((element 2)1))
			(and	(setf	(assoc 'template element) 
					(push (string (list2xml ((element 2) 1))) '(template) -1))))
		(expr2append-xml element 1) 
		(append-file export-file "\n"))
	(append-file export-file {</aiml>}))

(define (response-exists? pattern-input)
	(setq pattern-input (upper-case pattern-input)) 
	(if (dolist (entry responses) (= (lookup 'pattern entry) pattern-input))))

(define (newrp pattern-input template-input)
	(setq pattern-input (upper-case pattern-input))
	(if (catch (ends-with template-input "=")) 
		(throw-error "If you use \" in your input use \{\} on the outside.")) 
	(if (catch(response-exists? pattern-input)) 
		(throw-error "Pattern already exists!") 
		(set-newrp pattern-input template-input) )) 

(define (newrp2t pattern-input that-input template-input)
        (setq pattern-input (upper-case pattern-input))
	(println "ARGS? WTF? " (args))
        (if (catch (ends-with template-input "="))
                (throw-error "If you use \" in your input use \{\} on the outside."))
        (if (catch(response2that-exists? pattern-input))
                (throw-error "Pattern already exists!")
                (set-newrp pattern-input that-input template-input) ))


(define (rp2xml pattern-input template-input)
	(setq pattern-input (upper-case pattern-input)) 
	(expr2xml (list 'category (cons 'pattern pattern-input) (cons 'template template-input)))) 

(define (set-topic str)
	(if (catch(not(string? str))) (throw-error "set-topic <input> needs to be a string!"))
	(setq topic (upper-case str))
	(setq export-file (append ((exec "echo $HOME")0) "/AIML/" export-filename "_" topic ".aiml"))
	(replace "exported" export-file-header (string "exported_" topic))
	(println "new topic: " topic ", export-file: " export-file) true) 

(define (save-exit) (save save-data) (println "data saved, exit...") (exit))

(help)
;; ToDo List
;; Hinzufügen, Reset Responses, Kategorien zusammenfassen, Random erledig? 
;; check ob output korrektes xml ist 
;; that fehlt, muss bevor template
;; export needs topic
;; different responses list per topic
;; Sonderzeichen aussortieren





