#lang scheme/base

(require test-engine/scheme-tests
         "anormalize.ss"
         "../../collects/moby/runtime/stx.ss")
(require (for-syntax scheme/base
                     "../../stx-helpers.ss"
                     "../../collects/moby/runtime/stx.ss"))


#|
This file has several test cases in it. The first is a walk-through of the entire process,
detailing the input and output at each stage as the code goes through the high-level functions in each
file. The rest are simply input-output pairs that I wrote by hand and verified are correct (and work
properly).

Also, I'm writing everything as s-expressions. If you use stx->datum on all outputs, this is what you
will get.
|#


(define-syntax (check-expansion stx)
  (syntax-case stx ()
    [(_ original-form expanded-form)
     (with-syntax ([a-stx-sexp (stx->sexp (syntax->stx #'original-form))])
       (syntax/loc stx
         (check-expect (stx->datum 
                        (anormalize (sexp->stx (quote a-stx-sexp))))
                       (quote expanded-form))))]))


(check-expansion [(define (f x)
                    (* x x))]
                 
                 [(define (d0_f a_x)
                    (* a_x a_x))])


(check-expansion [(define (g x)
                    x)]
                 
                 [(define (d0_g a_x)
                    a_x)])


(test)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#;(


;; Walk through:

;; Starting code, this is a basic memoize function that demonstrates most of the different aspects of the transformation.

'((define (memoize f)
    (local [(define-struct entry (key value))
            (define table (box empty))]
      (lambda (n)
        (local [(define lookup
                  (filter (lambda (result) (equal? (entry-key result) n))
                          (unbox table)))]
          (if (empty? lookup)
              (local [(define ans (f n))]
                (begin
                  (set-box! table (cons (make-entry n ans) (unbox table)))
                  ans))
              (entry-value (first lookup))))))))

;; First we munge the identifiers in a relatively straight-forward way. That returns:

;; This is the output of munge-identifiers (file: munge-ids.ss) on the original input.
;; Differences are every identifier bound in this code

'((define (d0_memoize a_f)
    (local [(define-struct s1_entry (key value))
            (define d2_table (box empty))]
      (lambda (a_n)
        (local [(define d3_lookup
                  (filter (lambda (a_result) (equal? (s1_entry-key a_result) a_n))
                          (unbox d2_table)))]
          (if (empty? d3_lookup)
              (local [(define d4_ans (a_f a_n))]
                (begin
                  (set-box! d2_table (cons (make-s1_entry a_n d4_ans) (unbox d2_table)))
                  d4_ans))
              (s1_entry-value (first d3_lookup))))))))

;; After munging identifiers, we lift all locally defined struct definitions to top level
;; just by pulling them out. This is necessary because when we start fragmenting procedures
;; we may need the struct in  several different fragments, so it's easiest to have it at toplevel.
;; Also, because we already munged identifiers, we can guarantee that there won't be name collisions.
;; We do this now because it's as easy a time as any.

;; This is the output of lift-struct-defs (file: elim-anon.ss) on the original input.
;; Differences are just the lifted definition

'((define-struct s1_entry (key value))
  
  (define (d0_memoize a_f)
    (local [(define d2_table (box empty))]
      (lambda (a_n)
        (local [(define d3_lookup
                  (filter (lambda (a_result) (equal? (s1_entry-key a_result) a_n))
                          (unbox d2_table)))]
          (if (empty? d3_lookup)
              (local [(define d4_ans (a_f a_n))]
                (begin
                  (set-box! d2_table (cons (make-s1_entry a_n d4_ans) (unbox d2_table)))
                  d4_ans))
              (s1_entry-value (first d3_lookup))))))))

;; After munging identifiers, we name all anonymous procedures.
;; This is necessary because we will likely need to fragment some or all of them,
;; and they need a name to be fragmented. This effectively removes all instances
;; of the word "lambda" and uses local definitions instead.

;; This is the output of mapping name-anormal-procs (file: elim-anon.ss)
;;    across the output of list-struct-defs

'((define-struct s1_entry (key value))
  
  (define (d0_memoize a_f)
    (local [(define d2_table (box empty))
            (define (anon0 a_n)
              (local [(define d3_lookup
                        (local [(define (anon1 a_result) (equal? (s1_entry-key a_result) a_n))]
                          (filter anon1 (unbox d2_table))))]
                (if (empty? d3_lookup)
                    (local [(define d4_ans (a_f a_n))]
                      (begin
                        (set-box! d2_table (cons (make-s1_entry a_n d4_ans) (unbox d2_table)))
                        d4_ans))
                    (s1_entry-value (first d3_lookup)))))]
      anon0)))

;; Now we do something a bit more complicated.
;; We take all local definitions that are not explicitly procedures
;;    and change them to being defined as "(box 'undefined)", and then set them
;;    in a begin after the definitions block.
;; The reason for this is to avoid oddities with mutual references during fragmentation.
;; I don't do this for things that are obviously procedure definitions because
;;    nothing is being evaluated yet so it's unnecessary (though it would work if I did it).
;; Finally, since the local definitions are now boxes, we need to wrap things with unbox.

;; This it the output of read-anormalize (file: box-local-defs.ss) on the original input
;; Differences at the definitions of d2_table, d3_lookup, d4_ans, and any references to those

'((define-struct s1_entry (key value))
  
  (define (d0_memoize a_f)
    (local [(define (anon0 a_n)
              (local [(define d3_lookup (box 'undefined))]
                (begin
                  (set-box! d3_lookup
                            (local [(define (anon1 a_result) (equal? (s1_entry-key a_result) a_n))]
                              (filter anon1 (unbox (unbox d2_table)))))
                  (if (empty? (unbox d3_lookup))
                      (local [(define d4_ans (box 'undefined))]
                        (begin
                          (set-box! d4_ans (a_f a_n))
                          (begin
                            (set-box! (unbox d2_table) (cons (make-s1_entry a_n (unbox d4_ans))
                                                             (unbox (unbox d2_table))))
                            (unbox d4_ans))))
                      (s1_entry-value (first (unbox d3_lookup)))))))
            
            (define d2_table (box 'undefined))]
      (begin
        (set-box! d2_table (box empty))
        anon0))))

;; At this point we're finally ready for the actual a-normalizing.
;; This is just putting everything in a-normal form in a pretty standard way

;; This is the output of anormalize (file: anormalize.ss) on the original input
;; Differences in the BEGIN statements after definitions of d3_lookup and d4_ans

'((define-struct s1_entry (key value))
  
  (define (d0_memoize a_f)
    (local [(define (anon0 a_n)
              (local [(define d3_lookup (box 'undefined))]
                (begin
                  (local [(define temp0
                            (local [(define (anon1 a_result) (equal? (s1_entry-key a_result) a_n))]
                              (filter anon1 (unbox (unbox d2_table)))))]
                    (set-box! d3_lookup temp0))
                  (if (empty? (unbox d3_lookup))
                      (local [(define d4_ans (box 'undefined))]
                        (begin
                          (local [(define temp1 (a_f a_n))]
                            (set-box! d4_ans temp1))
                          (begin
                            (set-box! (unbox d2_table) (cons (make-s1_entry a_n (unbox d4_ans))
                                                             (unbox (unbox d2_table))))
                            (unbox d4_ans))))
                      (s1_entry-value (first (unbox d3_lookup)))))))
            (define d2_table (box 'undefined))]
      (begin
        (set-box! d2_table (box empty))
        anon0))))

;; Finally we get to the last step: fragmenting the code.
;; We fragment a procedure whenever there's a local definition that is not boxed junk (that we put in)
;;    or a procedure definition (since those aren't being evaluated), though the procedures
;;    will also be fragmented.
;; We also fragment each instruction of a begin and each expression in and/or statements.

;; This it the output of fragment (file: fragmenter.ss) on the original input
;; Differences are ubiquitous, though most of the fragmenting is done on anon0

'((define-struct s1_entry (key value))
  
  (define (f0_d0_memoize anon0 d2_table a_f) anon0)
  
  (define (d0_memoize a_f)
    (local [(define (f4_anon0 d4_ans d3_lookup a_n) (unbox d4_ans))
            
            (define (f3_anon0 d4_ans d3_lookup a_n)
              (begin
                (set-box! (unbox d2_table) (cons (make-s1_entry a_n (unbox d4_ans))
                                                 (unbox (unbox d2_table))))
                (f4_anon0 d4_ans d3_lookup a_n)))
            
            (define (f2_anon0 temp1 d4_ans d3_lookup a_n) (set-box! d4_ans temp1))
            
            (define (f1_anon0 d3_lookup a_n)
              (if (empty? (unbox d3_lookup))
                  (local [(define d4_ans (box 'undefined))]
                    (begin
                      (local [(define temp1 (a_f a_n))]
                        (f2_anon0 temp1 d4_ans d3_lookup a_n))
                      (f3_anon0 d4_ans d3_lookup a_n)))
                  (s1_entry-value (first (unbox d3_lookup)))))
            
            (define (f0_anon0 temp0 d3_lookup a_n)
              (set-box! d3_lookup temp0))
            
            (define (anon0 a_n)
              (local [(define d3_lookup (box 'undefined))]
                (begin
                  (local [(define temp0
                            (local [(define (anon1 a_result) (equal? (s1_entry-key a_result) a_n))]
                              (filter anon1 (unbox (unbox d2_table)))))]
                    (f0_anon0 temp0 d3_lookup a_n))
                  (f1_anon0 d3_lookup a_n))))
            
            (define d2_table (box 'undefined))]
      (begin
        (set-box! d2_table (box empty))
        (f0_d0_memoize anon0 d2_table a_f)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****** More Test Cases ****** ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I'm not walking through these since that would be extremely long and tedious,
;; but here are two more input-output pairs of fragment to look at.

;; 1

;; Input:
'((define (my-foldr f base lst)
    (if (empty? lst)
        base
        (f (first lst) (my-foldr f base (rest lst)))))
  
  (define (my-map f lst)
    (foldr (lambda (elt rst) (cons (f elt) rst))
           empty
           lst))
  
  (define (transpose matrix)
    (if (or (empty? matrix)
            (empty? (first matrix)))
        empty
        (cons (my-map first matrix)
              (transpose (my-map rest matrix))))))

;; Output
'((define (f0_d0_my-foldr temp0 a_f a_base a_lst)
    (a_f (first a_lst) temp0))
  
  (define (d0_my-foldr a_f a_base a_lst)
    (if (empty? a_lst)
        a_base
        (local [(define temp0 (d0_my-foldr a_f a_base (rest a_lst)))]
          (f0_d0_my-foldr temp0 a_f a_base a_lst))))
  
  
  (define (d1_my-map a_f a_lst)
    (local [(define (f0_anon0 temp1 a_elt a_rst)
              (cons temp1 a_rst))
            (define (anon0 a_elt a_rst)
              (local [(define temp1 (a_f a_elt))]
                (f0_anon0 temp1 a_elt a_rst)))]
      (foldr anon0 empty a_lst)))
  
  
  (define (f3_d2_transpose temp4 temp3 temp2 temp5 a_matrix)
    (cons temp3 temp4))
  
  (define (f2_d2_transpose temp3 temp2 temp5 a_matrix)
    (local [(define temp4 (d2_transpose temp2))]
      (f3_d2_transpose temp4 temp3 temp2 temp5 a_matrix)))
  
  (define (f1_d2_transpose temp2 temp5 a_matrix)
    (local [(define temp3 (d1_my-map first a_matrix))]
      (f2_d2_transpose temp3 temp2 temp5 a_matrix)))
  
  (define (f0_d2_transpose temp5 a_matrix)
    (if temp5
        empty
        (local [(define temp2 (d1_my-map rest a_matrix))]
          (f1_d2_transpose temp2 temp5 a_matrix))))
  
  (define (d2_transpose a_matrix)
    (local [(define (f0_temp5) (empty? (first a_matrix)))
            (define temp5 (or (empty? a_matrix) (f0_temp5)))]
      (f0_d2_transpose temp5 a_matrix))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 2

;; Input
'((define (max lst)
    (if (empty? (rest lst))
        (first lst)
        (local [(define max-rest (max (rest lst)))]
          (if (<= (first lst) max-rest)
              (first lst)
              max-rest)))))

;; Output
'((define (f1_d0_max d1_max-rest a_lst)
    (if (<= (first a_lst) (unbox d1_max-rest))
        (first a_lst)
        (unbox d1_max-rest)))
  
  (define (f0_d0_max temp0 d1_max-rest a_lst)
    (set-box! d1_max-rest temp0))
  
  (define (d0_max a_lst)
    (if (empty? (rest a_lst))
        (first a_lst)
        (local [(define d1_max-rest (box 'undefined))]
          (begin
            (local [(define temp0 (d0_max (rest a_lst)))]
              (f0_d0_max temp0 d1_max-rest a_lst))
            (f1_d0_max d1_max-rest a_lst))))))
)