#lang scheme/base
(require scheme/class
         scheme/contract
         scheme/match
         (planet schematics/spgsql/spgsql)
         (only-in srfi/19 current-date date?))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data structures

(define-struct model (db))

(define-struct owner (id email) #:transparent)

(define-struct project (id name owner is-validated?) #:transparent #:mutable)

(define-struct source (id version code date) #:transparent)

(define-struct compiler (id name version) #:transparent)

(define-struct compilation (id
                            compiler-id
                            date-started
                            date-finished
                            source-id) #:transparent)
(define-struct (binary-compilation compilation) (result) #:transparent)
(define-struct (error-compilation compilation) (error-message) #:transparent)

(define-struct download (id date ip) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define current-model 
  (make-parameter #f))


;; current-db: -> connection
;; Returns the current connection associated to the current model.
(define (current-db)
  (model-db (current-model)))



;; new-model: -> model
;; Creates a new model.
(define (new-model #:user user
                   #:database database
                   #:password [password #f] 
                   #:server [server "localhost"]
                   #:port [port 5432])
  (let ([db (connect #:server server
                     #:user user
                     #:database database
                     #:password password
                     #:port port)])
    (make-model db)))


;; last-insert-id: string -> number
;; Postgresql specific: returns the identifier last used to insert into a table.
;; Takes the sequence name.
(define (last-insert-id seqname)
  (let ([db (current-db)])
    (vector-ref ((send db prepare-query-row "select currval($1)")
                 seqname)
                0)))


;; owner-add!: string -> owner
;; Adds a new owner to the model.
(define (owner-add! email)
  (let* ([db (current-db)]
         [stmt (send db prepare-exec "insert into owner (email) values ($1)")])
    (stmt email)
    (make-owner (last-insert-id "owner_id_seq") email)))


;; owner-lookup/email: string -> (or/c owner #f)
(define (owner-lookup/email email)
  (let* ([db (current-db)]
         [query (send db prepare-query-maybe-row 
                     "select id, email from owner where email = $1")])
    (cond [(query email)
           =>
           (lambda (row)
             (make-owner
              (vector-ref row 0)
              (vector-ref row 1)))]
          [else
           #f])))


;; owner-lookup/id: number -> (or/c owner #f)
(define (owner-lookup/id id)
  (let* ([db (current-db)]
         [query (send db prepare-query-maybe-row 
                     "select id, email from owner where id = $1")])
    (cond [(query id)
           =>
           (lambda (row)
             (make-owner
              (vector-ref row 0)
              (vector-ref row 1)))]
          [else
           #f])))


;; owner-projects: owner -> (listof project)
;; Returns the owners associated to a project.
(define (owner-projects an-owner)
  (let* ([db (current-db)]
         [query (send db prepare-map "select id, name, is_validated
                                      from project where owner_id = $1"
                      (lambda (id name is-validated?)
                        (make-project id name an-owner is-validated?)))])
    (query (owner-id an-owner))))
    


;; project-add!: owner string -> project
;; Adds a new project, associated to an owner.
(define (project-add! an-owner name)
  (let* ([db (current-db)]
         [stmt (send db prepare-exec 
                     "insert into project (name, is_validated, owner_id)
                      values ($1, $2, $3)"
                     )])
    (stmt name #f (owner-id an-owner))
    (make-project (last-insert-id "project_id_seq")
                  name 
                  an-owner
                  #f)))

;; project-validate!: project -> void
;; Validates a given project.
(define (project-validate! a-project (value #t))
  (let* ([db (current-db)]
         [stmt (send db prepare-exec 
                     "update project set is_validated=$1 where id=$2")])
    (stmt value
          (project-id a-project))
    (set-project-is-validated?! a-project value)))


;; project-sources: project -> (listof source)
;; Produces the sources attached to a project.
(define (project-sources a-project)
  (let* ([db (current-db)]
         [query (send db prepare-map "select id, version, code, date
                                     from source
                                     where project_id = $1
                                     order by version"
                     (lambda (id version code date)
                       (make-source id version code (sql-datetime->srfi-date date))))])
    (query (project-id a-project))))


;; source-lookup/id: number -> (or/c source #f)
;; Looks up a source by its id.
(define (source-lookup/id an-id)
  (let* ([db (current-db)]
         [query (send db prepare-query-maybe-row "select id, version, code, date
                                     from source
                                     where id = $1")])
    (cond [(query an-id)
           =>
           (lambda (a-row)
             (apply (lambda (id version code date) 
                      (make-source id version code (sql-datetime->srfi-date date))) 
                    (vector->list a-row)))]
          [else #f])))


;; source-add!: project bytes -> source
(define (source-add! project code)
  (let* ([db (current-db)]
         [an-sql-timestamp (srfi-date->sql-timestamp (current-date))]
         [stmt (send db prepare-exec
                    "insert into source (date, version, code, project_id)
                     select $1,
                            coalesce(max(version)+1, 1),
                            $2,
                            $3
                     from source")])
    (stmt an-sql-timestamp code (project-id project))
    (source-lookup/id (last-insert-id "source_id_seq"))))


;; compiler-add!: string number -> compiler
(define (compiler-add! name version)
  (let* ([db (current-db)]
         [stmt (send db prepare-exec
                     "insert into compiler (name, version)
                      values ($1, $2)")])
    (stmt name version)
    (make-compiler (last-insert-id "compiler_id_seq") name version)))


;; compilers: (listof compiler)
;; Produces a list of all the compilers known by the model.
(define (compilers)
  (let* ([db (current-db)]
         [query (send db prepare-map "select id, name, version
                                     from compiler
                                     order by name, version"
                     (lambda (id name version)
                       (make-compiler id name version)))])
    (query)))


;; compiler-lookup/id: number -> (or/c compiler #f)
;; Produces a list of all the compilers known by the model.
(define (compiler-lookup/id an-id)
  (let* ([db (current-db)]
         [query (send db prepare-query-maybe-row 
                      "select id, name, version
                       from compiler
                       where id = $1")])
    (cond [(query an-id)
           =>
           (lambda (a-row)
             (make-compiler (vector-ref a-row 0) 
                            (vector-ref a-row 1) 
                            (vector-ref a-row 2)))]
          [else #f])))


;; source-compilations: source -> compilation
;; Returns a list of compilations.
(define (source-compilations a-source)
  (let* ([db (current-db)]
         [query (send db prepare-map "select id, 
                                             type,
                                             compiler_id,
                                             date_started,
                                             date_finished,
                                             source_id,
                                             result,
                                             error
                                      from compilation
                                      where source_id = $1"
                      (lambda (id type compiler-id
                                  date-started date-finished
                                  source-id result error-message)
                        (match type 
                          ["binary"
                           (make-binary-compilation 
                            id compiler-id 
                            (sql-datetime->srfi-date date-started)
                            (sql-datetime->srfi-date date-finished)
                            source-id result)]
                          ["error"
                           (make-error-compilation 
                            id compiler-id 
                            (sql-datetime->srfi-date date-started)
                            (sql-datetime->srfi-date date-finished)
                            source-id error-message)])))])
    (query (source-id a-source))))


;; compilation-lookup/id: number -> (or/c compilation #f)
(define (compilation-lookup/id an-id)
  (let* ([db (current-db)]
         [query (send db prepare-map "select id, 
                                             type,
                                             compiler_id,
                                             date_started,
                                             date_finished,
                                             source_id,
                                             result,
                                             error
                                      from compilation
                                      where id = $1"
                      (lambda (id type compiler-id
                                  date-started date-finished
                                  source-id result error-message)
                        (match type 
                          ["binary"
                           (make-binary-compilation 
                            id compiler-id 
                            (sql-datetime->srfi-date date-started)
                            (sql-datetime->srfi-date date-finished)
                            source-id result)]
                          ["error"
                           (make-error-compilation 
                            id compiler-id 
                            (sql-datetime->srfi-date date-started)
                            (sql-datetime->srfi-date date-finished)
                            source-id error-message)])))])
    (match (query an-id)
      [(list a-compilation)
       a-compilation]
      [else 
       #f])))



;; compilation-compiler: compilation -> compiler
(define (compilation-compiler a-compilation)
  (compiler-lookup/id (compilation-compiler-id a-compilation)))


;; binary-compilation-add!: date date compiler source bytes -> binary-compilation
(define (binary-compilation-add! date-started date-finished a-compiler a-source a-result)
    (compilation-add! "binary" date-started date-finished a-compiler a-source a-result sql-null))

;; error-compilation-add!: date date compiler source string -> error-compilation
(define (error-compilation-add! date-started date-finished a-compiler a-source an-error-message)
  (compilation-add! "error" date-started date-finished a-compiler a-source sql-null an-error-message))
 
;; compilation-add!: (one-of "binary" "error") date date compiler source (or/c bytes sql-null) (or/c string sql-null) -> compilation 
(define (compilation-add! type date-started date-finished a-compiler a-source a-result an-error-message)
  (let* ([db (current-db)]
         [stmt (send db prepare-exec
                     "insert into compilation (type, 
                                               compiler_id,
                                               date_started,
                                               date_finished,
                                               source_id,
                                               result,
                                               error)
                      values ($1,            -- type
                              $2,            -- compiler-id
                              $3,            -- date-started
                              $4,            -- date-finished
                              $5,            -- source-id
                              $6,            -- a-result
                              $7             -- an-error-message
                              )")])
    (stmt type
          (compiler-id a-compiler)
          (srfi-date->sql-timestamp date-started)
          (srfi-date->sql-timestamp date-finished)
          (source-id a-source)
          a-result
          an-error-message)
    (compilation-lookup/id (last-insert-id "compilation_id_seq"))))




;; compilation-download: compilation -> (listof download)
;; Returns the download record.
(define (compilation-downloads a-compilation)
  (let* ([db (current-db)]
         [query (send db prepare-map "select id, date, ip from download where compilation_id=$1"
                      (lambda (id date ip) 
                        (make-download id (sql-datetime->srfi-date date) ip)))])
    (query (compilation-id a-compilation))))


;; download-add!: compilation ip -> download
;; Records a current download
(define (download-add! a-compilation an-ip)
  (let* ([db (current-db)]
         [a-datetime (current-date)]
         [stmt (send db prepare-exec "insert into download(date, ip, compilation_id)
                                      values ($1, $2, $3)")])
    (stmt (srfi-date->sql-timestamp a-datetime) 
          an-ip
          (compilation-id a-compilation))
    (make-download (last-insert-id "download_id_seq")
                   a-datetime
                   an-ip)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; just for exercising.
(define (boot)
  (current-model (new-model #:user "dyoo" #:database "j2me-world")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide current-model)

(provide/contract [owner-email (owner? . -> . string?)]
                  [owner-add! (string? . -> . owner?)]
                  [owner-lookup/email (string? . -> . (or/c owner? false/c))]
                  [owner-lookup/id (natural-number/c . -> . (or/c owner? false/c))]
                  [owner-projects (owner? . -> . (listof project?))]

                  [project-name (project? . -> . string?)]
                  [project-add! (owner? string? . -> . project?)]
                  [project-validate! (project? . -> . any)]
                  [project-sources (project? . -> . (listof source?))]
                  
                  [source-version (source? . -> . natural-number/c)]
                  [source-code (source? . -> . bytes?)]
                  [source-date (source? . -> . date?)]
                  [source-add! (project? bytes? . -> . source?)]
                  [source-compilations (source? . -> . (listof compilation?))]
                  
                  [compiler-name (compiler? . -> . string?)]
                  [compiler-version (compiler? . -> . natural-number/c)]
                  [compiler-add! (string? natural-number/c . -> . compiler?)]
                  [compilers (-> (listof compiler?))]
                                    
                  [compilation-compiler (compilation? . -> . compiler?)]
                  [compilation-date-started (compilation? . -> . date?)]
                  [compilation-date-finished (compilation? . -> . date?)]
                  [binary-compilation-result (binary-compilation? . -> . bytes?)]
                  [error-compilation-error-message (error-compilation? . -> . string?)]
                  [binary-compilation-add! (date? date? compiler? source? bytes? . -> . binary-compilation?)]
                  [error-compilation-add! (date? date? compiler? source? string? . -> . error-compilation?)]
                  [compilation-downloads (compilation? . -> . (listof download?))]

                  [download-ip (download? . -> . string?)]
                  [download-date (download? . -> . date?)]
                  [download-add! (compilation? string? . -> . download?)]
                  )
