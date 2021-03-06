!# /user/bin/env newlisp

; startup variables are: topic, savefile-name, path, data-dir exportfile-name, export-file, export-file-header, indentation, last-backup, namepattern
; *-name is the file with the descriptor, *-file is the filename with the path
; standard topic is "none"

(setq categories '())
(setq topic "none")
(setq data-dir "/AIML/")
(setq savefile-name (string "categories" ".lsp")) ;standard, set-topic will change this
(setq path (append ((exec "echo $HOME")0) data-dir))
(load (append path savefile-name)) ;loads standard categories, set-topic will load others
(setq namepattern "exported")
(setq exportfile-name (string "exported" ".aiml")) ;set-topic will change that
(setq export-file (append path exportfile-name))
(setq export-file-header {<aiml version="1.0.1" encoding="UTF-8">
<!-- exported.aiml -->})
(load "expr2append-xml.lsp")
(setq indentation 4)
(setq indentation (dup " " indentation))

;;backup
(setq last-backup (- (date-value) (integer (first(parse (first(sort (exec (append "ls " path "/backups"))>))".")))))
(if (> last-backup 1000) (save (append path "backups/" (string(date-value)) ".lsp") 'categories))
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

;; functions to create new aiml categories, update or alter them
;;
(define (help)
	(println "--------------------(help) for this overview------------------------------------") 
	(println "commands : [newcat] [update-cat] [save-data] [cat2xml] [expr2xml] [list2xml]")
	(println "commands : [that4question] []")
	(println "commands : [set-topic] [random-list] [help] [reload] [export] [save-exit]")
	(println "--------------------------------------------------------------------------------")
	(println "debugging: [category-exists?] [category2that-exists?] [category-replace]")
	(println "debugging: [no-special-chars?]")
	(println "debugging: [debuging only!: set-newcat, set-newthat]") 
	(println "path+file: " path "\t" "export-file: " export-file)
	(println "--------------------------------------------------------------------------------") true)

(define (reload) (! "clear") (load "expr2aiml.lsp") true)

(define (list2xml input) 
	(if	(catch (list? input)) 
		(and	(setq output "<random>\n")
			(dolist (element input)
				(push (string (dup indentation 4) "<li>" element "</li>\n") output -1))  
			(push (string (dup indentation 3) "</random>") output -1) output)
		(throw-error "Input in list2xml wasn't a list...")))

(define (no-special-chars? input) 
	(= '() (clean true? (map (fn (x) (find (get-char(upper-case x)) chars)) (explode input)))))

(define (set-newcat pattern-input template-input)
	(if (catch (no-special-chars? pattern-input))
		(throw-error "special chars in the pattern causes errors")
		(setq pattern-input (upper-case pattern-input)))
	(if (string? template-input)
		(push (list 'category (cons 'pattern pattern-input) (cons 'template template-input)) categories -1)
		(push (list 'category (cons 'pattern pattern-input) (cons 'template (list template-input))) categories -1))
	(last categories))

(define (update-cat pattern-input template-input)
	(setq pattern-input (upper-case pattern-input))
	(setq counter -1) 
	(dolist (entry categories) 
		(inc counter) (if (= (lookup 'pattern entry) pattern-input) 
		(setf (assoc 'template (categories counter)) (cons 'template template-input)) ))
	(last categories))

(define (save-data) (save (append path savefile-name) 'categories))

(define (category-replace element) 
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

(define (export option) 
	(write-file export-file export-file-header)
	; (if (and topic (!= topic "") (!= "*"))  ) unnecessary?
	(append-file export-file "\n\n")
	(dolist (element categories)
		(replace "srai:" element {<srai>})
		(replace ":srai" element {</srai>})
		(replace "-sr/-" element {<sr/>})
                (replace "sr::" element {<srai>})
                (replace "::sr" element {</srai>})
		(replace "-star/-" element {<star/>})
		(if (find "::" element) (category-replace element))
		(if (list? ((element 2)1))
			(and	(setf	(assoc 'template element) 
					(push (string (list2xml ((element 2) 1))) '(template) -1))))
		(expr2append-xml element 1) 
		(append-file export-file "\n"))
	(append-file export-file {</aiml>})
	(if (= option "return") (! (string "cat " export-file))) true)

(define (category-exists? pattern-input)
	(setq pattern-input (upper-case pattern-input)) 
	(if (dolist (entry categories) (= (lookup 'pattern entry) pattern-input))))

(define (newcat pattern-input template-input)
        (if (catch (no-special-chars? pattern-input)) 
                (throw-error "special chars in the pattern causes errors")
		(setq pattern-input (upper-case pattern-input)))
	(if (catch (ends-with template-input "=")) 
		(throw-error "If you use \" in your input use \{\} on the outside.")) 
	(if (catch (category-exists? pattern-input)) 
		(throw-error "Pattern already exists! Use update-cat to change it.") 
		(set-newcat pattern-input template-input) )) 

(define (newthat pattern-input that-input template-input)
        (setq pattern-input (upper-case pattern-input))
        (if (catch (or(ends-with template-input "=") (ends-with that-input "=")))
                (throw-error "If you use \" in your input use \{\} on the outside."))
        (if (catch (and (category-exists? pattern-input) (that-exists?)))
                (throw-error "Pattern already exists! Use update-that to change it.")
                (set-newthat pattern-input that-input template-input) ))

(define (that-exists? pattern-input that-input)
        (setq pattern-input (upper-case pattern-input))
	(setq that-input (upper-case that-input))
        (if (dolist (entry categories) 
		(and	(= (lookup 'pattern entry) pattern-input))
			(= (lookup 'that entry) that-input)) ))

(define (update-that pattern-input that-input template-input)
        (setq pattern-input (upper-case pattern-input))
        (setq counter -1)
        (dolist (entry categories)
                (inc counter) (if (= (lookup 'pattern entry) pattern-input)
                (setf (assoc 'that (categories counter)) (cons 'that that-input))
		(setf (assoc 'template (categories counter)) (cons 'template template-input))) 
        (last categories)))

(define (set-newthat pattern-input that-input template-input)
        (setq pattern-input (upper-case pattern-input))
        (if (and (string? that-input) (string? template-input)) 
                (push (list 'category (cons 'pattern pattern-input) (cons 'that that-input) (cons 'template template-input)) categories -1)
                (push (list 'category (cons 'pattern pattern-input) (cons 'that that-input) (cons 'template (list template-input))) categories -1))
        (last categories))

(define (cat2xml pattern-input template-input)
	(setq pattern-input (upper-case pattern-input)) 
	(expr2xml (list 'category (cons 'pattern pattern-input) (cons 'template template-input)))) 

(define (set-topic str)
	(if (catch(not(string? str))) (throw-error "set-topic <input> needs to be a string!"))
	(setq topic (upper-case str))
	(setq export-file (append path (string(first(parse exportfile-name ".")) "_" topic ".aiml")))
	(replace "exported" export-file-header (string "exported_" topic))
	(println "new topic: " topic ", export-file: " export-file) true) 

(define (that4question) 
	(dolist (cat categories) 
		(when (ends-with (lookup 'template cat) "?") 
			(println (assoc 'pattern cat))
			(println (lookup 'template cat)))) true)

(define (save-exit) (save save-data) (println "data saved, exit...") (exit))

(help)
(println "topic is: " topic)

;; ToDo List

; set-topic isn't finished, nameing mess...
; set-topic changes export path, but export doesnt work after that, export-file in expr2append-xml?
; set-topic: different categories list per topic
; filter special chars from <that> newthat input? 
; Add something, eg. to a random list
; reset/delete categories
; join categories? eg. make a random list out of the responses  
; fork whole topics based on mood, sentiment or context, mktemp, live-import
; live update of new files while running
; newthat uses only that-exists? which only checks if a pattern and that is there, not a normal cat.
; automatic creation of 'that' for every question in the pattern, to handle the answer, that4question
; export should have a show option to show "cat" the file after creation

;;later
; recognizing automatically when a new topic should be set for some input, eg. category-exists?
; set-topic: export needs topic, so only that file would need to be create
; 

;; less important
; check if output is correct xml? With tidy? Other linter? 




