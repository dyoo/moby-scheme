#lang scribble/doc
@(require scribble/manual)

@title{SQLite: An interface to SQLite databases}

By Jay McCarthy (jay at cs dot byu dot edu) & Noel Welsh (noelwelsh at GMail)

SQLite gives you access to SQLite database in PLT Scheme.

@section{API}

@defmodule[(planet jaymccarthy/sqlite:3)]

@defstruct[exn:sqlite ([message string?] [continuation-marks continuation-mark-set?]) #:transparent]{
 This exception is thrown whenever the library encounters an
 error.  The @scheme[message] field gives more information about the
 error.
}

@defproc[(db? [x any/c]) boolean?]{
 Identifies if @scheme[x] represents an SQLite database.
}

@defproc[(open [db-path (or/c path-string? (symbols ':memory: ':temp:))]) db?]{
 Opens the SQLite database at @scheme[db-path].
       
 If @scheme[':memory:] or @scheme[':temp:] are passed they correspond to the
 string arguments @scheme[":memory:"] and @scheme[""] to SQLite's open function.
 These correspond to a private, temporary in-memory database and a private, temporary
 on-disk database.
}

@defproc[(close [db db?]) void]{
 Closes the database referred to by @scheme[db].
}

@defproc[(exec [db db?] [sql string?]
               [callback ((listof vector?) (listof vector?) . -> . integer?)])
         void]{
Executes @scheme[sql] with the given @scheme[db], calling @scheme[callback] for
each row of the results.  @scheme[callback] is passed two
vectors, one of the column names and one of the column
values.  @scheme[callback] returns an integer status code.  If
the status code is anything other than zero execution halts
with an exception. If the query does not return results, @scheme[callback] will not be called.
}

@defproc[(exec/ignore [db db?] [sql string?])
         void]{
A wrapper around @scheme[exec] that provides a void callback.
}

@defproc[(insert [db db?] [sql string?])
         integer?]{
Executes @scheme[sql] with the @scheme[db].  The query is assumed to be an
INSERT statement, and the result is the ID of the last row
inserted.  This is useful when using AUTOINCREMENT or
INTEGER PRIMARY KEY fields as the database will choose a
unique value for this field

If the SQL is not an insertion statement it is still
executed, the results if any are discarded, and the returned
value is unspecified.
}

@defproc[(select [db db?] [sql string?])
         (listof vector?)]{
Executes @scheme[sql] with the given @scheme[db], collating the results in to
a list where each element is a vector of the columns values
as strings.  The first vector contains the column names.  If
the statement returns no results an empty list is returned.
}

@defproc[(prepare [db db?] [sql string?])
         statement?]{
Compiles @scheme[sql] into a statement object for the given
@scheme[db]. The query may contain ``?'' to mark a parameter.  Make
sure you free the statement after use with @scheme[finalize].  A
statement can be reused by calling @scheme[reset].
}

@defproc[(load-params [stmt statement?] [param any/c] ...)
         void]{
Loads @scheme[param]s into @scheme[stmt], filling in the `?'s.
}
              
@defproc[(step [stmt statement?]) (or/c vector? false/c)]{

Steps @scheme[stmt] to the next result, returning the column
values as a vector, or @scheme[#f] if the statement does not
return values or there are no more values.  Unlike @scheme[select],
values are converted to the appropriate Scheme type:

A NULL becomes @scheme[#f]. An INTEGER becomes an integer. A FLOAT becomes an inexact number. A STRING or TEXT becomes a string. A BLOB becomes a bytes.
}

@defproc[(step* [stmt statement?])
         (listof vector?)]{
Runs @scheme[step] until it is done collecting the results in a
list.  Use this rather than @scheme[select] or @scheme[exec] when you want to
use a placeholder (?) in the query and have SQLite do the
quoting for you.
}

@defproc[(run [stmt statement] [param any/c] ...)
         void]{

Loads the @scheme[param]s in the statement, then runs the
statement. (If the statement returns results, they are not
available.) (Use for UPDATE and INSERT)
}

@defproc[(reset [stmt statement?])
         void]{
Resets a statement for re-execution.
}
              
@defproc[(finalize [stmt statement?])
         void]{
Releases the resources held by a statement.
}

@defproc[(with-transaction* [db db?]
                            [lock-type (symbols 'none 'deferred 'immediate 'exclusive)]
                            [action ((-> void) . -> . any/c)])
         any/c]{

Runs @scheme[action] in a transaction in the given database with
the given lock type, returning the result of the action.
The action is passed a function of one argument which aborts
the transaction when called.  If the transaction is aborted
the result of the with-transaction* expression is the value
passed to the abort function.  If control leaves the action
via an exception or other continuation jump (i.e. without
action exiting normally) the transaction is aborted.
                         
Refer to the @link["http://www.sqlite.org/lockingv3.html"]{SQLite documentation} for the meaning of the lock-types.
}

@defform[(with-transaction (db fail) body ...)]{
 Equivalent to:
 @scheme[(with-transaction* db 'none (lambda (fail) body ...))].
}

@defform[(with-transaction/lock (db lock-type fail) body ...)]{
 Equivalent to:
 @scheme[(with-transaction* db lock-type (lambda (fail) body ...))].
}

@defproc[(errmsg [db db?])
         string?]{
Returns the message for the last error with the database.
}
                 
@defproc[(changes-count [db db?])
         integer?]{
Returns a count of how many rows were changed by the most
recently completed INSERT, UPDATE, or DELETE statement.
}
                  
@defproc[(total-changes-count [db db?])
         integer?]{
Returns a count of how many changes have been made to the
database since its creation.
}

@section{Notes}

If you encounter unexpected errors with the message "SQLite
Error: The database file is locked" check you haven't got
any un-finalized statements around.
