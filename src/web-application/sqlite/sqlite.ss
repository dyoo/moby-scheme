#lang scheme
(require (lib "plt-match.ss")
         (except-in (lib "contract.ss") ->)
         (rename-in (lib "contract.ss") 
                    [-> c->])
         "sqlite-ffi.ss")

; Struct
(define-struct (exn:sqlite exn) ())
(define-struct db (_db_ptr _errMsg_ptr _callback_ptr) #:mutable)
(define (db-handle db)
  (ptr-ref (db-_db_ptr db) _sqlite3_ptr))
(define-struct statement (_stmt_ptr) #:mutable)
(define (statement-handle stmt)
  (ptr-ref (statement-_stmt_ptr stmt) _sqlite3_stmt_ptr))

; Contracts  
(provide SQLITE_OK
         SQLITE_ERROR)
(provide/contract
 [db-handle (db? . c-> . cpointer?)]
 [statement-handle (statement? . c-> . cpointer?)]
 [open ((or/c path? (symbols ':memory: ':temp:)) . c-> . db?)]
 [close (db? . c-> . void?)]
 [exec (db? string? (vector? vector? . c-> . integer?) . c-> . void?)]
 [exec/ignore (db? string? . c-> . void?)]
 [insert (db? string? . c-> . integer?)]
 [select (db? string? . c-> . (listof vector?))]
 [prepare (db? string? . c-> . statement?)]
 [load-params ((statement?) (listof (union string? number? bytes?)) . ->* . (void?))]
 [step (statement? . c-> . (union (vectorof (union string? number? false/c bytes?)) false/c))]
 [run ((statement?) (listof (union string? number? bytes?)) . ->* . (void?))]
 [reset (statement? . c-> . void?)]
 [finalize (statement? . c-> . void?)]
 [errmsg (db? . c-> . string?)]
 [changes-count (db? . c-> . integer?)]
 [total-changes-count (db? . c-> . integer?)]
 [step* (statement? . c-> . (listof (vectorof (union string? number? false/c bytes?))))]
 [with-transaction* (db? (symbols 'none 'deferred 'immediate 'exclusive)  (any/c . c-> . any/c) . c-> . any/c)])

(provide with-transaction/lock
         with-transaction
         (struct-out exn:sqlite)
         statement?
         db?)

; Library Helpers
(define (list->equal-sublists n l)
  (let loop ([l l] [i 0] [r `()])
    (if (null? l)
        (reverse r)
        (loop (cdr l) (modulo (+ i 1) n)
              (if (= i 0)
                  (append (list (list (car l)))
                          r)
                  (append (list (append (car r) (list (car l))))
                          (cdr r)))))))

;; cvector->list-of-vector : cvector natural natural -> (list-of vector)
;;
;; Convert a cvector holding a matrix of values with the
;; given number of rows and columns to a list of vectors
(define (cvector->list-of-vector rows columns cvector)
  (define (column->vector r)
    (let ((vec (make-vector columns)))
      (let loop ((c 0))
        (if (= c columns)
            vec
            (begin
              (vector-set!
               vec
               c
               (cvector-ref cvector (+ (* r columns) c)))
              (loop (add1 c)))))))
  ;; sqlite3_get_table is slightly inconsistent.  Normally
  ;; there will be one more row than reported (see the
  ;; SQLite 3 documentation) except if the select
  ;; statement returns no results, in which case there
  ;; will be no columns and rows.  Hence we check for this
  ;; here.
  (if (zero? columns)
      null
      (let loop ((r 0))
        (if (= r rows)
            null
            (cons
             (column->vector r)
             (loop (add1 r)))))))

;; cvector->vector : cvector -> vector
(define (cvector->vector cvec)
  (let ((vec (make-vector (cvector-length cvec))))
    (let loop ((i (cvector-length cvec)))
      (if (zero? i)
          vec
          (begin
            (vector-set! vec (sub1 i) (cvector-ref cvec (sub1 i)))
            (loop (sub1 i)))))))

(define (wrap-finalizer o f)
  (register-finalizer o f)
  o)

;; handle-status : (U db #f) integer -> integer
;;
;; Returns the status code if no error occurred, otherwise
;; raises an exception with an appropriate message.
(define (handle-status db s)
  (if (or (= s SQLITE_OK)
          (= s SQLITE_ROW)
          (= s SQLITE_DONE))
      s
      (raise (make-exn:sqlite
              (string->immutable-string
               (format "SQLite Error: ~a"
                       (lookup-status-message db s)))
              (current-continuation-marks)))))

;; lookup-status-message : (U db #f) integer -> string
(define (lookup-status-message db s)
  (if (and (eq? s SQLITE_ERROR) db)
      (errmsg db)
      (cdr (assoc s
                  `([,SQLITE_ERROR . "Generic error, perhaps call errmsg?"]
                    [,SQLITE_INTERNAL . "An internal logic error in SQLite"]
                    [,SQLITE_PERM . "Access permission denied"]
                    [,SQLITE_ABORT . "Callback routine requested an abort"]
                    [,SQLITE_BUSY . "The database file is locked"]
                    [,SQLITE_LOCKED . "table in the database is locked"]
                    [,SQLITE_NOMEM . "A malloc() failed"]
                    [,SQLITE_READONLY . "Attempt to write a readonly database"]
                    [,SQLITE_INTERRUPT . "Operation terminated by sqlite3_interrupt()"]
                    [,SQLITE_IOERR . "Some kind of disk I/O error occurred"]
                    [,SQLITE_CORRUPT . "The database disk image is malformed"]
                    [,SQLITE_NOTFOUND . "(Internal Only) Table or record not found"]
                    [,SQLITE_FULL . "Insertion failed because database is full"]
                    [,SQLITE_CANTOPEN . "Unable to open the database file"]
                    [,SQLITE_PROTOCOL . "Database lock protocol error"]
                    [,SQLITE_EMPTY . "Database is empty"]
                    [,SQLITE_SCHEMA . "The database schema changed"]
                    [,SQLITE_TOOBIG . "Too much data for one row of a table"]
                    [,SQLITE_CONSTRAINT . "Abort due to contraint violation"]
                    [,SQLITE_MISMATCH . "Data type mismatch"]
                    [,SQLITE_MISUSE . "Library used incorrectly"]
                    [,SQLITE_NOLFS . "Uses OS features not supported on host"]
                    [,SQLITE_AUTH . "Authorization denied"]
                    [,SQLITE_FORMAT . "Auxiliary database format error"]
                    [,SQLITE_RANGE . "2nd parameter to sqlite3_bind out of range"]
                    [,SQLITE_NOTADB . "File opened that is not a database file"])))))

; Methods
(define (open db-path)
  (let ([r (wrap-finalizer (make-db (malloc _sqlite3_ptr_ptr)
                                    (malloc _string_ptr)
                                    (malloc _pointer))
                           close)]
        [db-path (cond
                   [(symbol? db-path)
                    (case db-path
                      ; Private, temporary in-memory
                      [(:memory:) #":memory:"]
                      ; Private, temporary on-disk
                      [(:temp:) #""])]                   
                   [(relative-path? db-path)
                    (path->bytes (build-path (current-directory) db-path))]
                   [else
                    (path->bytes db-path)])])
    (when (handle-status r (sqlite3_open db-path (db-_db_ptr r)))
      r)))

(define (close db)
  (let ([o-_db_ptr (db-_db_ptr db)])
    (set-db-_db_ptr! db #f)
    (set-db-_errMsg_ptr! db #f)
    (set-db-_callback_ptr! db #f)
    (when o-_db_ptr
      (handle-status db (sqlite3_close (db-handle (make-db o-_db_ptr #f #f)))))
    (void)))

(define (insert db sql)
  (exec/ignore db sql)
  (sqlite3_last_insert_rowid (db-handle db)))

(define (exec db sql callback)
  (let ([exec-callback
         (lambda (arg_ptr column-count_int column-values_ptr column-names_ptr)
           (callback
            (cvector->vector
             (make-cvector* column-names_ptr _string column-count_int))
            (cvector->vector
             (make-cvector* column-values_ptr _string column-count_int))))])
    (handle-status
     db
     (sqlite3_exec (db-handle db)
                   sql
                   (contract
                    (cpointer? integer? cpointer? cpointer? . c-> . integer?)
                    exec-callback
                    'positive 'negative)
                   (db-_callback_ptr db)
                   (db-_errMsg_ptr db)))
    (void)))
(define (exec/ignore db sql)
  (exec db sql (lambda (c v) 0)))

(define (select db sql)
  (let ([result_ptr_ptr
         (wrap-finalizer (malloc _string_array_ptr) 
                         (lambda (ptr)
                           (when ptr
                             (sqlite3_free_table
                              (ptr-ref ptr _string_array)))))]
        [row-count_ptr (malloc _int_ptr)]
        [column-count_ptr (malloc _int_ptr)])
    (when (handle-status
           db
           (sqlite3_get_table (db-handle db)
                              sql
                              result_ptr_ptr
                              row-count_ptr
                              column-count_ptr
                              (db-_errMsg_ptr db)))
      (cvector->list-of-vector
       (add1 (ptr-ref row-count_ptr _int))
       (ptr-ref column-count_ptr _int)
       (make-cvector* (ptr-ref result_ptr_ptr _string_array)
                      _string 
                      (* (+ (ptr-ref row-count_ptr _int) 1)
                         (ptr-ref column-count_ptr _int)))))))

(define (prepare db sql)
  (let ([stmt (wrap-finalizer (make-statement (malloc _sqlite3_stmt_ptr_ptr))
                              finalize)])
    (when (handle-status
           db
           (sqlite3_prepare (db-handle db)
                            sql
                            (string-length sql)
                            (statement-_stmt_ptr stmt)
                            (db-_errMsg_ptr db)))
      (if  (zero? (ptr-ref (statement-_stmt_ptr stmt) _int))
           ;; the pointer is null; SQLite didn't raise an
           ;; error but should have!
           (handle-status db SQLITE_ERROR)
           stmt))))

(define (load-params stmt . params)
  (let* ((handle (statement-handle stmt))
         (parameter-count (sqlite3_bind_parameter_count handle)))
    (when (not (= (length params) parameter-count))
      (raise-mismatch-error
       'load-params
       (format "Given ~a params when statement requires ~a params" (length params) parameter-count)
       params))
    (begin
      (reset stmt)
      (let loop ((i 1) (params params))
        (if (null? params)
            (void)
            (begin
              (let* ([param (car params)])
                (handle-status
                 #f
                 (cond
                   [(integer? param)
                    (sqlite3_bind_int64 handle i param)]
                   [(number? param)
                    (sqlite3_bind_double handle i param)]
                   [(string? param)
                    (sqlite3_bind_text handle i param)]
                   [(bytes? param)
                    (sqlite3_bind_blob handle i param)]
                   [else
                    (sqlite3_bind_null handle i)])))
              (loop (add1 i) (cdr params))))))))

(define (step stmt)
  (let ([s (handle-status
            #f
            (sqlite3_step (statement-handle stmt)))])
    (cond
      [(= s SQLITE_ROW)
       (let* ((cols (sqlite3_column_count (statement-handle stmt)))
              (vec (make-vector cols))
              (handle (statement-handle stmt)))
         (let loop ((i 0))
           (if (= i cols)
               vec
               (begin
                 (vector-set!
                  vec
                  i
                  (match (sqlite3_column_decltype handle i)
                    ["NULL" #f]
                    ["INTEGER" (sqlite3_column_int64 handle i)]
                    ["FLOAT" (sqlite3_column_double handle i)]
                    [(or #f "STRING" "TEXT")
                     (sqlite3_column_text handle i)]
                    ["BLOB" (sqlite3_column_blob handle i)]))
                 (loop (add1 i))))))]
      [(= s SQLITE_DONE)
       #f])))

(define (run stmt . params)
  (begin (apply load-params stmt params)
         (handle-status #f (sqlite3_step (statement-handle stmt)))
         (void)))

(define (reset stmt)
  (handle-status #f (sqlite3_reset (statement-handle stmt)))
  (void))

(define (finalize stmt)
  (let ([o-_stmt_ptr (statement-_stmt_ptr stmt)])
    (set-statement-_stmt_ptr! stmt #f)
    (when o-_stmt_ptr
      (handle-status #f (sqlite3_finalize (statement-handle (make-statement o-_stmt_ptr)))))
    (void)))

(define (lock-type->string lock-type)
  (case lock-type
    [(none) ""]
    [(deferred) "DEFERRED"]
    [(immediate) "IMMEDIATE"]
    [(exclusive) "EXCLUSIVE"]))

;; with-transaction* : db symbol (ec -> 'a) -> 'a
(define (with-transaction* db lock-type body-f)
  (let ((end #f))
    (dynamic-wind
     (lambda ()
       (set! end (lambda ()
                   (exec/ignore db "ROLLBACK TRANSACTION")))
       (exec/ignore db
                    (format "BEGIN ~a TRANSACTION"
                            (lock-type->string lock-type))))
     (lambda ()
       (let/ec fail
         (begin0
           (body-f fail)
           (set! end
                 (lambda ()
                   (exec/ignore db "COMMIT TRANSACTION"))))))
     (lambda ()
       (end)))))

(define-syntax with-transaction/lock
  (syntax-rules ()
    [(_ (db lock-type fail) body ...)
     (with-transaction* db 'lock-type (lambda (fail) body ...))]))
(define-syntax with-transaction
  (syntax-rules ()
    [(_ (db fail) body ...)
     (with-transaction/lock (db none fail) body ...)]))

(define (errmsg db)
  (sqlite3_errmsg (db-handle db)))
(define (changes-count db)
  (sqlite3_changes (db-handle db)))
(define (total-changes-count db)
  (sqlite3_total_changes (db-handle db)))

; User Helpers
(define (step* stmt)
  (let loop ([r (list)])
    (let ([c (step stmt)])
      (if (not c)
          (reverse r)
          (loop (cons c r))))))