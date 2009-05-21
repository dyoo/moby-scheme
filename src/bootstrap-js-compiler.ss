#lang scheme/base
(require (only-in scheme/list empty? empty first rest)
         "beginner-to-javascript.ss"
         "helpers.ss")

;; Bootstrap the javascript compiler.
;; 
;; * Ignores all provide/contracts
;; * Concatenates all the required modules into a single file
;; * Compiles the javascript compiler with the javascript compiler.


;; bootstrap-compile: path -> compiled-program
(define (bootstrap-compile a-path)
  (let* ([modules (find-transitive-required-modules a-path)]
         [big-program (apply append (map (lambda (p)
                                           (remove-requires
                                            (remove-provide/contracts
                                             (read-program p))))
                                         modules))])
    (program->compiled-program big-program)))
    



;; find-transitive-required-modules: path -> (listof path)
(define (find-transitive-required-modules a-path)
  (unique
   (let loop ([a-path a-path])
     (let ([new-paths 
            (get-require-paths (read-program a-path))])
       (cond
         [(empty? new-paths)
          (list a-path)]
         [else
          (append
           (apply append
                  (map loop new-paths))
           (list a-path))])))))
     


;; read-program: path -> program
(define (read-program a-path)
  (call-with-input-file a-path
    (lambda (ip)
      (check-special-lang-line! (read-line ip)) ;; skip the first language-level line
      (let loop ([elt (read ip)])
        (cond
          [(eof-object? elt)
           empty]
          [else
           (cons elt (loop (read ip)))])))))


;; make sure the line is a #lang s-exp "lang.ss" line.
(define (check-special-lang-line! a-line)
  (unless (regexp-match #rx"^#lang s-exp \"lang.ss\"$" a-line)
    (error 'check-special-line!)))





;; get-require-paths: program -> (listof module-path)
;; Produces the module paths that are required in the program.
(define (get-require-paths a-program)
  (cond
    [(empty? a-program)
     empty]
    [(library-require? (first a-program))
     (append (rest (first a-program))
             (get-require-paths (rest a-program)))]
    [else
     (get-require-paths (rest a-program))]))


;; remove-provide/contracts: program -> program
(define (remove-provide/contracts a-program)
  (filter (lambda (top-level)
            (not (list-begins-with? top-level 'provide/contract)))
          a-program))


;; remove-requires: program -> program
(define (remove-requires a-program)
  (filter (lambda (top-level)
            (not (list-begins-with? top-level 'require)))
          a-program))


;; unique: (listof X) -> (listof X)
;; Produces a unique list of the elements, assuming elements can be
;; compared with equal? and are hashable.
(define (unique elts)
  (let ([ht (make-hash)])
    (let loop ([elts elts])
      (cond
        [(empty? elts)
         empty]
        [(hash-ref ht (first elts) #f)
         (loop (rest elts))]
        [else
         (hash-set! ht (first elts) #t)
         (cons (first elts)
               (loop (rest elts)))]))))
