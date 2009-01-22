;;;
;;; Time-stamp: <06/01/03 14:28:46 nhw>
;;;
;;; Copyright (C) 2005 by Noel Welsh.
;;;

;;; This library is free software; you can redistribute it
;;; and/or modify it under the terms of the GNU Lesser
;;; General Public License as published by the Free Software
;;; Foundation; either version 2.1 of the License, or (at
;;; your option) any later version.

;;; This library is distributed in the hope that it will be
;;; useful, but WITHOUT ANY WARRANTY; without even the
;;; implied warranty of MERCHANTABILITY or FITNESS FOR A
;;; PARTICULAR PURPOSE.  See the GNU Lesser General Public
;;; License for more details.

;;; You should have received a copy of the GNU Lesser
;;; General Public License along with this library; if not,
;;; write to the Free Software Foundation, Inc., 59 Temple
;;; Place, Suite 330, Boston, MA 02111-1307 USA

;;; Author: Noel Welsh <noelwelsh@yahoo.com>
;;
;;
;; Commentary:

(module util scheme

  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           (only-in (lib "base64.ss" "net") base64-encode-stream))
  
  ;; For dealing with serialised hash tables.
  (require
   (only-in (planet "sxml.ss" ("lizorkin" "sxml.plt" 2 0)) sxpath))

  (provide (all-defined-out))

  ;; RPC timeout
  ;; When working on the library without an internet connection (or, more
  ;; importantly, when your proxies are set up incorrectly), some of the RPC
  ;; tests should timeout sooner rather than later. And, they should 
  ;; "fail", although techincally it isn't exactly a failure.
  (define RPC-TIMEOUT 10)
  
  (define-syntax (with-timeout stx)
    (syntax-case stx ()
      [(_ timeout-msg time body)
       #`(let ()
           (define complete? #f)
           (define test-thread 
             (thread (lambda () (set! complete? body))))
           (define timeout-thread
             (thread (lambda () 
                       (sleep time)
                       (when (thread-running? test-thread)
                         (printf "Test timed out.~n")
                         (printf "Test skipped: ~a~n" timeout-msg)
                         (kill-thread test-thread)))))
           (define test-done (thread-dead-evt test-thread))
           (define kill-done (thread-dead-evt timeout-thread))
           
           ;; Wait for the test or kill to be done.
           (sync test-done kill-done)
           
           ;; If the test completed, that's good... but we
           ;; should cleanup the timeout thread.
           (when (thread-running? timeout-thread)
             (kill-thread timeout-thread))

           ;; Regardless of who killed whom, we return the state variable.
           complete?
           )]))
  
  ;; Failure check
  ;; I want to write some tests that are supposed to fail. These are,
  ;; I suppose, failure tests. I'll just wrap these up in a macro
  ;; to make life a little easier.
  (define-syntax (check-fail stx)
    (syntax-case stx ()
      [(_ check)
       #`(check-exn
          exn:test:check?
          (lambda ()
            check))]))
  
  (define-check (check-hash-table-equal? hash1 hash2)
    (check-hash-table-contains hash1 hash2)
    (check-hash-table-contains hash2 hash1))
  
  (define (set-equal? ls1 ls2)
    (define (check-set s1 s2)
      (andmap (lambda (o)
                (member o s2)) s1))
    (and (check-set ls1 ls2)
         (check-set ls2 ls1)))
  
  ;; check-serialised-hash-table-equal? : sxml sxml -> void
  ;; Unfortunately, the serialisation checks for hash tables don't
  ;; reliably give us ordering... so we have to do the same thing for
  ;; the serialised form as we do with hashes that are deserialised.
  ;;
  ;; They come in looking like:
  ;;
  ;; (value
  ;;  (struct
  ;;   (member (name "c") (value (double "3.0")))
  ;;   (member (name "b") (value (string "2")))
  ;;   (member (name "a") (value (int "1")))))
  ;; 
  ;; Therefore, I'll pull them apart like the lists that they are, and
  ;; run some checks over the names and values.
  (define-check (check-serialised-hash-table-equal? sxml1 sxml2)
    (define (extract-names sxml) ((sxpath '(// name)) sxml))
    (define (extract-values sxml) ((sxpath '(// value)) sxml))
    (let ([names1 (extract-names sxml1)]
          [names2 (extract-names sxml2)]
          [values1 (extract-values sxml1)]
          [values2 (extract-values sxml2)])

      ;; Make sure both hash tables have the same number of elements 
      ;; before proceeding.
      (when (not (equal? (length names1) (length names2)))
        (with-check-info
         (('message "Hash tables have different numbers of elements."))
         (fail-check)))
      
      ;; Check that both sets of keys are equal.
      (unless (set-equal? names1 names2)
        (with-check-info
         (('message 
           (format "First hash has different keys than second hash:~n\tH1: ~s~n\tH2 ~s~n"
                   names1 names2)))
         (fail-check)))
      
      ;; Do the same (unordered) check for values.
      (unless (set-equal? values1 values2)
        (with-check-info
         (('message 
           (format "First hash has different values than second hash:~n\tH1: ~s~n\tH2 ~s~n"
                   values1 values2)))
         (fail-check)))
      
      ;; And lastly, do a hash-table check on them... because I don't want to implement
      ;; it using association lists. So, I'll load each of these lists of names and values
      ;; into a hash table, and then use 'check-hash-table-equal?' on them. It's a bit
      ;; of a cheat, and feels round-about... but it's a step.
      (let ([h1 (make-hash)]
            [h2 (make-hash)])
        (define (load-hash h lon lov)
          (for-each (lambda (n v)
                      (hash-set! h (string->symbol (cadr n)) v))
                    lon lov))
        (load-hash h1 names1 values1)
        (load-hash h2 names2 values2)
        
        (unless (check-hash-table-equal? h1 h2)
          (with-check-info 
           (('message "Serialised hash tables are not equal."))
           (fail-check))))
            
        ))
  
  ;; check-hash-table-contains : hash-table hash-table -> void
  ;;
  ;; Assert that hash1 contains all the values in hash2.
  ;; I.e. that hash1 is equal to or a superset of hash2.
  (define-check (check-hash-table-contains hash1 hash2)
    (hash-for-each
     hash2
     (lambda (key v2)
       (let ((v1
              (hash-ref
               hash1 key
               (lambda ()
                 (with-check-info
                  (('message
                    (format "No value found with key ~e" key)))
                  (fail-check))))))
         (if (and (hash? v1) (hash? v2))
             (check-hash-table-equal?* v1 v2)
             (check-equal? v1 v2))))))

  ;; base64-encode : bytes -> string
  (define (base64-encode byte)
    (let ((output (open-output-string)))
      (base64-encode-stream
       (open-input-bytes byte)
       output
       #"")
      (get-output-string output)))
  
  )
