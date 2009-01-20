#lang scheme
(require (lib "foreign.ss"))
(unsafe!)
(provide (all-from-out (lib "foreign.ss"))
         (all-defined-out))

(define sqlite (ffi-lib "libsqlite3"))

; Syntax Helpers
(define-syntax define-sqlite
  (syntax-rules ()
    [(_ id type)
     (define id (get-ffi-obj (quote id) sqlite type))]))

; Types
(define _sqlite3_ptr _pointer)
(define _sqlite3_ptr_ptr _pointer)
(define _sqlite3_stmt_ptr _pointer)
(define _sqlite3_stmt_ptr_ptr _pointer)

(define _sqlite_callback (_fun _pointer _int _pointer _pointer -> _int))
(define _sqlite3_busy_handler (_fun _pointer _int -> _int))

(define _string_ptr _pointer)
(define _string_array _pointer)
(define _string_array_ptr _pointer)
(define _int_ptr _pointer)

; Functions
(define-sqlite sqlite3_libversion_number (_fun -> _int))
(define-sqlite sqlite3_close (_fun _sqlite3_ptr -> _int))
(define-sqlite sqlite3_exec (_fun _sqlite3_ptr _string _sqlite_callback _string_array _string_array -> _int))
(define-sqlite sqlite3_last_insert_rowid (_fun _sqlite3_ptr -> _int64))
(define-sqlite sqlite3_changes (_fun _sqlite3_ptr -> _int))
(define-sqlite sqlite3_total_changes (_fun _sqlite3_ptr -> _int))
(define-sqlite sqlite3_interrupt (_fun _sqlite3_ptr -> _void))
(define-sqlite sqlite3_complete (_fun _string -> _int))
; sqlite3_complete16
; sqlite3_busy_handler
; sqlite3_busy_timeout
(define-sqlite sqlite3_get_table (_fun _sqlite3_ptr _string _string_array_ptr _int_ptr _int_ptr _pointer -> _int))
(define-sqlite sqlite3_free_table (_fun _string_array -> _void))
; sqlite3_ printf functions
; sqlite3_set_authorizer
; sqlite3_trace
; sqlite3_progress_handler
; sqlite3_commit_hook
(define-sqlite sqlite3_open (_fun _bytes _sqlite3_ptr_ptr -> _int))
; sqlite3_open16
(define-sqlite sqlite3_errcode (_fun _sqlite3_ptr -> _int))
(define-sqlite sqlite3_errmsg (_fun _sqlite3_ptr -> _string))
; sqlite3_errmsg16
(define-sqlite sqlite3_prepare (_fun _sqlite3_ptr _string _int _sqlite3_stmt_ptr_ptr _string_ptr -> _int))
; sqlite3_prepare16
; sqlite3_bind functions
(define-sqlite sqlite3_bind_int (_fun _sqlite3_stmt_ptr _int _int -> _int))
(define-sqlite sqlite3_bind_int64 (_fun _sqlite3_stmt_ptr _int _int64 -> _int))
(define-sqlite sqlite3_bind_double (_fun _sqlite3_stmt_ptr _int _double -> _int))
(define-sqlite sqlite3_bind_text (_fun (stmt col the-string) ::
                                       (stmt : _sqlite3_stmt_ptr)
                                       (col : _int)
                                       (string-ptr : _string = the-string)
                                       (string-len : _int = (string-utf-8-length the-string))
                                       (flag : _int = -1)
                                       -> _int))
(define-sqlite sqlite3_bind_blob (_fun (stmt col the-bytes) ::
                                       (stmt : _sqlite3_stmt_ptr)
                                       (col : _int)
                                       (byte-ptr : _bytes = the-bytes)
                                       (byte-len : _int = (bytes-length the-bytes))
                                       (flag : _int = -1)
                                       -> _int))
(define-sqlite sqlite3_bind_null (_fun _sqlite3_stmt_ptr _int -> _int))
;(_fun -> _void) -> _int))
(define-sqlite sqlite3_bind_parameter_count (_fun _sqlite3_stmt_ptr -> _int))
(define-sqlite sqlite3_bind_parameter_name (_fun _sqlite3_stmt_ptr _int -> _string))
(define-sqlite sqlite3_bind_parameter_index (_fun _sqlite3_stmt_ptr _string -> _int))
; sqlite3_clear_bindings
(define-sqlite sqlite3_column_count (_fun _sqlite3_stmt_ptr -> _int))
(define-sqlite sqlite3_column_name (_fun _sqlite3_stmt_ptr _int -> _string))
; sqlite3_column_name16
(define-sqlite sqlite3_column_decltype (_fun _sqlite3_stmt_ptr _int -> _string))
; sqlite3_column_decltype16
(define-sqlite sqlite3_step (_fun _sqlite3_stmt_ptr -> _int))
(define-sqlite sqlite3_data_count (_fun _sqlite3_stmt_ptr -> _int))
; sqlite3_column functions
(define-sqlite sqlite3_column_int (_fun _sqlite3_stmt_ptr _int -> _int))
(define-sqlite sqlite3_column_int64 (_fun _sqlite3_stmt_ptr _int -> _int64))
(define-sqlite sqlite3_column_double (_fun _sqlite3_stmt_ptr _int -> _double))
(define-sqlite sqlite3_column_text (_fun _sqlite3_stmt_ptr _int -> _string))
(define-sqlite sqlite3_column_bytes (_fun _sqlite3_stmt_ptr _int -> _int))
(define-sqlite sqlite3_column_blob (_fun (stmt : _sqlite3_stmt_ptr)
                                         (col : _int)
                                         -> (blob : _bytes)
                                         -> (let ([len (sqlite3_column_bytes stmt col)])
                                              (make-sized-byte-string blob len))))
(define-sqlite sqlite3_finalize (_fun _sqlite3_stmt_ptr -> _int))
(define-sqlite sqlite3_reset (_fun _sqlite3_stmt_ptr -> _int))
; sqlite3_ user-defined function functions
(define-sqlite sqlite3_expired (_fun _sqlite3_ptr -> _int))
;(define-sqlite sqlite3_global_recover (_fun -> _int))

; Constants
(define SQLITE_OK           0   ) ; Successful result */
(define SQLITE_ERROR        1   ) ; SQL error or missing database */
(define SQLITE_INTERNAL     2   ) ; An internal logic error in SQLite */
(define SQLITE_PERM         3   ) ; Access permission denied */
(define SQLITE_ABORT        4   ) ; Callback routine requested an abort */
(define SQLITE_BUSY         5   ) ; The database file is locked */
(define SQLITE_LOCKED       6   ) ; A table in the database is locked */
(define SQLITE_NOMEM        7   ) ; A malloc() failed */
(define SQLITE_READONLY     8   ) ; Attempt to write a readonly database */
(define SQLITE_INTERRUPT    9   ) ; Operation terminated by sqlite3_interrupt()*/
(define SQLITE_IOERR       10   ) ; Some kind of disk I/O error occurred */
(define SQLITE_CORRUPT     11   ) ; The database disk image is malformed */
(define SQLITE_NOTFOUND    12   ) ; (Internal Only) Table or record not found */
(define SQLITE_FULL        13   ) ; Insertion failed because database is full */
(define SQLITE_CANTOPEN    14   ) ; Unable to open the database file */
(define SQLITE_PROTOCOL    15   ) ; Database lock protocol error */
(define SQLITE_EMPTY       16   ) ; Database is empty */
(define SQLITE_SCHEMA      17   ) ; The database schema changed */
(define SQLITE_TOOBIG      18   ) ; Too much data for one row of a table */
(define SQLITE_CONSTRAINT  19   ) ; Abort due to contraint violation */
(define SQLITE_MISMATCH    20   ) ; Data type mismatch */
(define SQLITE_MISUSE      21   ) ; Library used incorrectly */
(define SQLITE_NOLFS       22   ) ; Uses OS features not supported on host */
(define SQLITE_AUTH        23   ) ; Authorization denied */
(define SQLITE_FORMAT      24   ) ; Auxiliary database format error */
(define SQLITE_RANGE       25   ) ; 2nd parameter to sqlite3_bind out of range */
(define SQLITE_NOTADB      26   ) ; File opened that is not a database file */
(define SQLITE_ROW         100  ) ; sqlite3_step() has another row ready */
(define SQLITE_DONE        101  ) ; sqlite3_step() has finished executing */

(define SQLITE_INTEGER  1)
(define SQLITE_FLOAT    2)
(define SQLITE3_TEXT    3)
(define SQLITE_BLOB     4)
(define SQLITE_NULL     5)

(define SQLITE_STATIC     0)
(define SQLITE_TRANSIENT -1)