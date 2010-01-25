#lang scheme/base

(require (only-in scheme/list empty? empty first rest)
         scheme/runtime-path
         scheme/path
         scheme/port
         scheme/file
         scheme/pretty
         scheme/local
         (only-in scheme/list second)
         "compile-helpers.ss"
         "program-resources.ss"
         "collects/runtime/stx.ss"
         "collects/runtime/binding.ss"
         "compiler/beginner-to-javascript.ss"
         "compiler/desugar.ss"
         "compiler/analyzer.ss"
         "compiler/helpers.ss"
         "compiler/pinfo.ss"
         "compiler/modules.ss")

(require (for-syntax (only-in scheme/base build-path)))

;; Bootstrap the runtime components of Moby, as well as the
;; Scheme->Javascript compiler in support/js/compiler.js.
;; 
;;
;; * For each library, concatenates all the required modules into a single file.
;;
;; * Compiles the javascript compiler with the javascript compiler.



;; A module record bundles together the name and path of a module.
(define-struct module-record (name path))


;; These are modules that will be bootstrapped to be used by at runtime.
(define COLLECTS-PATH "collects")


;; Here's the list of runtime modules.
(define RUNTIME-MODULES
  (list (make-module-record 'moby/runtime/runtime-modules
                            (build-path COLLECTS-PATH "runtime" "runtime-modules.ss"))
        (make-module-record 'moby/runtime/stx 
                            (build-path COLLECTS-PATH "runtime" "stx.ss"))
        (make-module-record 'moby/runtime/binding
                            (build-path COLLECTS-PATH "runtime" "binding.ss"))
        (make-module-record 'moby/runtime/permission-struct
                            (build-path COLLECTS-PATH "runtime" "permission-struct.ss"))
        (make-module-record 'moby/runtime/effect-struct 
                            (build-path COLLECTS-PATH "runtime" "effect-struct.ss"))
        (make-module-record 'moby/runtime/arity-struct 
                            (build-path COLLECTS-PATH "runtime" "arity-struct.ss"))
        (make-module-record 'moby/runtime/error-struct 
                            (build-path COLLECTS-PATH "runtime" "error-struct.ss"))
        (make-module-record 'moby/runtime/error-struct-to-dom 
                            (build-path COLLECTS-PATH "runtime" "error-struct-to-dom.ss"))
        
        
        (make-module-record 'bootstrap/bootstrap-teachpack 
                            (build-path COLLECTS-PATH "bootstrap" "bootstrap-teachpack.ss"))
        (make-module-record 'bootstrap/cage-teachpack 
                            (build-path COLLECTS-PATH "bootstrap" "cage-teachpack.ss"))
        (make-module-record 'bootstrap/function-teachpack 
                            (build-path COLLECTS-PATH "bootstrap" "function-teachpack.ss"))))



(define-runtime-path moby-runtime-path
  "../support/js/runtime")

(define-runtime-path runtime-manifest-path
  "../support/js/runtime/MANIFEST")


(define-runtime-path
  compiler-path
  "../support/js/runtime/compiler.js")


(define-runtime-path
  standalone-compiler-parent-path
  "../support/js/standalone-compiler")

;; The standalone compiler combines the sources of the regular compiler
;; and its dependent libraries.
(define-runtime-path
  standalone-compiler-path
  "../support/js/standalone-compiler/standalone-compiler.js")


(define-runtime-path
  compressed-standalone-compiler-path
  "../support/js/standalone-compiler/compressed-standalone-compiler.js")



(define-runtime-path base.js  "../support/js/runtime/base.js")
(define-runtime-path jshashtable.js  "../support/js/runtime/jshashtable.js")
(define-runtime-path types.js "../support/js/runtime/types.js")
(define-runtime-path kernel.js "../support/js/runtime/kernel.js")
(define-runtime-path read.js "../support/js/runtime/read.js")

(define-runtime-path compressed-runtime.js "../support/js/runtime/compressed-runtime.js")



  
;; write-compressed-runtime: -> void
;; Write out a runtime of all of the files in the MANIFEST, compressed by the YUI compressor.
(define (write-compressed-runtime)
  (let* ([runtime-source (get-runtime-source)]
         [compressed-runtime-source (compress-and-optimize-source runtime-source)])
    (call-with-output-file compressed-runtime.js
      (lambda (op) (write-bytes compressed-runtime-source op))
      #:exists 'replace))
  (void))


;; write-compressed-compilers: -> void
;; Writes a compressed version of the compiler and the standalone compiler.
(define (write-compressed-compilers)
  (let* ([runtime-source (file->bytes "../support/js/runtime/compiler.js")]
         [compressed-runtime-source (compress-and-optimize-source runtime-source)])
    (call-with-output-file "../support/js/runtime/compressed-compiler.js"
      (lambda (op) (write-bytes compressed-runtime-source op))
      #:exists 'replace))

  
  (let* ([standalone-source (file->bytes standalone-compiler-path)]
         [compressed-standalone-source (aggressively-compile-and-optimize-source standalone-source)])
    (call-with-output-file compressed-standalone-compiler-path
      (lambda (op) (write-bytes compressed-standalone-source op))
      #:exists 'replace))
  (void))



;; compress-and-optimize-source: bytes -> bytes
;; Apply some process for compressing and optimizing the Javascript.
(define (compress-and-optimize-source bytes)
  (google-closure-compile bytes)
  #;(yui-compress bytes))


;; aggressively-compile-and-optimize-source: bytes -> bytes
;; Compress the javascript very aggressively: used only for the standalone compiler, which
;; only exposes a single function 'compile' where everything else has been munged.
(define (aggressively-compile-and-optimize-source bytes)
  (google-closure-compile bytes #:aggressive? #t))




;; write-runtime-toplevel-bindings-descriptions: -> void
;; Write out compiler/gen/runtime-modules.ss, which describes the module binding
;; for the runtime modules listed in RUNTIME-MODULE-PATHS.

;; FIXME: we need to export permission information here too!
(define (write-runtime-toplevel-bindings-descriptions)
  (printf "Writing out the toplevel binding descriptions.~n")
  (printf "Warning: if collects/runtime/runtime-modules.ss does get changed, you may see an exception during the bootstrap.  You will~n")
  (printf "need to run the bootstrapper one more time to use the refreshed bindings.~n")
  (let ([moby-runtime-module-bindings-description
          `(define MOBY-RUNTIME-MODULE-BINDINGS
                  (list ,@
                  (for/list ([a-runtime-module (in-list RUNTIME-MODULES)])
                    (let* ([a-program+resources
                            (open-program/resources (module-record-path a-runtime-module))]
                           [desugared-program+pinfo 
                            (desugar-program (program/resources-program a-program+resources)
                                             (pinfo-update-current-module-path
                                              (get-base-pinfo 'base)
                                              (path->string (module-record-path a-runtime-module))))]
                           
                           [a-pinfo (program-analyze/pinfo (first desugared-program+pinfo)
                                                           (second desugared-program+pinfo))])
                      (list 'quote
                            (list
                             (module-record-name a-runtime-module)
                             (path->string (module-record-path a-runtime-module))
                             (map (lambda (a-binding) 
                                    (binding->sexp 
                                     (localize-binding-to-module 
                                      a-binding 
                                      (module-record-name a-runtime-module))))
                                  (pinfo-get-exposed-bindings a-pinfo))))))))])
    ;; POINT:
    ;; If we screw up computing the module-bindings-description, at least we won't get to this
    ;; point and overwrite runtime-modules.ss, since an exception will have happened up front.
    (call-with-output-file "collects/runtime/runtime-modules.ss"
      (lambda (op)
        (fprintf op "#lang s-exp \"private/restricted-runtime-scheme.ss\"\n")
        (fprintf op ";; This file is automagically generated and maintained by bootstrap-js-compiler.\n;; (in write-runtime-toplevel-bindings-descriptions)\n;; Do not edit this file by hand.\n")
        (pretty-print moby-runtime-module-bindings-description op)
        (newline op)
        (display '(provide MOBY-RUNTIME-MODULE-BINDINGS) op))
      
      #:exists 'replace)))


;; write-runtime-library-modules: -> void
;; Write out the runtime library.
(define (write-runtime-library-modules)
  (local [(define (get-js-target a-path-string)
            (string-append
             (substring (string-append "../support/js/runtime/" a-path-string)
                        0
                        (- (string-length 
                            (string-append "../support/js/runtime/" a-path-string))
                           3))
             ".js"))]
  (for ([a-runtime-module (in-list RUNTIME-MODULES)])
    (printf "Booting the runtime module ~s~n" (module-record-name a-runtime-module))
    (boot-compile-runtime-library 
     (module-record-name a-runtime-module)
     (path->string (module-record-path a-runtime-module))
     (get-js-target (path->string (module-record-path a-runtime-module)))))))


;; phase-1-bootstrap-compile: path-string -> string
;; The first phase of the bootstrapping compiles programs by treating require statements
;; as literal textual inclusion.
(define (phase-1-bootstrap-compile a-module-name a-path-string)
  (let ([pinfo-without-debugging-location-emits
         (pinfo-update-with-location-emits? (get-base-pinfo 'moby #;'base)
                                            #f)])
    (compiled-program-main/expose-as-module
     (program->compiled-program/pinfo (get-big-program a-path-string pinfo-without-debugging-location-emits)
                                      (pinfo-update-current-module-path
                                       pinfo-without-debugging-location-emits
                                       a-path-string))
     a-module-name)))





;; write-compiler: ->void
;; Writes out the javascript compiler and other files.
;; Generates: compiler.js, standalone-compiler.js, permission-struct.js
(define (write-compiler)
  (printf "Writing out the compiler~n")
  (boot-compile-runtime-library 'moby/compiler "compiler/beginner-to-javascript.ss" compiler-path)

 
  (unless (directory-exists? standalone-compiler-parent-path)
    (make-directory standalone-compiler-parent-path))
  (call-with-output-file standalone-compiler-path
    (lambda (op)
      (display "// This is the standalone compiler.\n" op)
      (display "// It's been automatically generated by bootstrap-js-compiler.ss\n" op)
      (display "// Please don't hand-edit this file.\n" op)
      (copy-path-to-port jshashtable.js op)
      (copy-path-to-port types.js op)
      (copy-path-to-port kernel.js op)
      (copy-path-to-port "../support/js/runtime/stx.js" op)
      (copy-path-to-port read.js op)
      (copy-path-to-port compiler-path op)
      #;(display (phase-1-bootstrap-compile "compiler/beginner-to-javascript.ss") op)
       
      (display "function listToArray(aList) {
           var anArray = [];
           while (!aList.isEmpty()) {     
               anArray.push(aList.first());
               aList = aList.rest();
           }
           return anArray;
           }
           var aPinfo = get_dash_base_dash_pinfo(plt.types.Symbol.makeInstance('moby'));"
               op)
      (display "// compileScheme: string -> (array string (arrayof string))\n" op)
      (display "
   function compileScheme(s) {
       var exprs = plt.reader.readSchemeExpressions(s, 'standalone');
       var compiledProgram =
           program_dash__greaterthan_compiled_dash_program_slash_pinfo(exprs, aPinfo);

       var compiledSrc = compiled_dash_program_dash_main(compiledProgram);
       var permList = pinfo_dash_permissions(compiled_dash_program_dash_pinfo(compiledProgram));
       var perms = [];
       while (!permList.isEmpty()) {     
           perms.push(
               permission_dash__greaterthan_string(permList.first()));
           permList = permList.rest();
       }
       return [compiledSrc, perms];
   }
   this['compileScheme'] = compileScheme;
   "
               op))
    #:exists 'replace))
      





;; boot-compile-runtime-library: module-name path path -> void
;; Write out the bootstrap-compilation of a Scheme program to a Javascript program.
(define (boot-compile-runtime-library a-module-name a-program-path an-output-path)
  (unless (directory-exists? (path-only an-output-path))
    (make-directory* (path-only an-output-path)))
  (call-with-output-file an-output-path
    (lambda (op)
      (display "// This is automatically generated by bootstrap-js-compiler.ss\n" op)
      (display "// Please don't hand-edit this file.\n" op)
      (display (phase-1-bootstrap-compile a-module-name a-program-path)
             op))
    #:exists 'replace))




;; get-big-program: path pinfo -> program
(define (get-big-program a-path a-pinfo)
  (let* ([modules (find-transitive-required-modules a-path a-pinfo)]
         [big-program (apply append (map (lambda (p)
                                           (remove-requires
                                            (read-program/forget-resources p)
                                            a-path
                                            p
                                            a-pinfo))
                                         modules))])
    big-program))
  



;; module-needs-inclusion?: path pinfo -> boolean
(define (module-needs-inclusion? a-path a-pinfo)
  (let ([path-resolver (pinfo-module-path-resolver a-pinfo)]
        [module-resolver (pinfo-module-resolver a-pinfo)])
    (not
     (and (module-name? (path-resolver a-path ""))
          (module-binding? (module-resolver (path-resolver a-path "")))))))


;; find-transitive-required-modules: path -> (listof path)
(define (find-transitive-required-modules a-path a-pinfo)
  (let ()
    (unique
     (let loop ([a-path a-path])
       (let ([new-paths 
              (filter (lambda (a-subpath)
                        (module-needs-inclusion? a-subpath a-pinfo))
                      (get-require-paths (read-program/forget-resources a-path)
                                         (path-only a-path)))])
         (cond
           [(empty? new-paths)
            (list a-path)]
           [else
            (append
             (apply append
                    (map loop new-paths))
             (list a-path))]))))))


;; read-program: path -> program
;; Given the path of a program, read the program in, discarding its
;; graphical resources.
(define (read-program/forget-resources a-path)
  (call-with-program/resources
   a-path
   (lambda (a-program/resources)
     (program/resources-program a-program/resources))))



;; get-require-paths: program path -> (listof module-path)
;; Produces the module paths that are required in the program.
(define (get-require-paths a-program base-path)
  (cond
    [(empty? a-program)
     empty]
    [(library-require? (first a-program))
     (append (map (lambda (x)
                    (let ([a-path (stx-e x)])
                      (cond [(string? a-path)
                             (path->string (build-path base-path a-path))]
                            [else
                             a-path])))
                  (rest (stx-e (first a-program))))
             (get-require-paths (rest a-program) base-path))]
    [else
     (get-require-paths (rest a-program) base-path)]))




;; remove-requires: program -> program
;; Removes the requires for a program.  However, leaves the ones
;; that are known to the compiler.
(define (remove-requires a-program parent-path a-subpath a-pinfo)
  (apply 
   append
   (map (lambda (top-level)
          (cond [(stx-begins-with? top-level 'require)
                 (cond
                   [(module-needs-inclusion? (module-path-join 
                                              parent-path
                                              (second (stx->datum top-level))) 
                                             a-pinfo)
                    (printf "    DEBUG: erasing the require statement ~s in ~s~n"
                            (stx->datum top-level)
                            a-subpath)
                    (list)]
                   [else
                    (printf "    DEBUG: preserving the require statement ~s in ~s~n"
                            (stx->datum top-level)
                            a-subpath)
                    (list top-level)
                    #;(let ([result
                           (list (datum->stx `(require ,(second (stx->datum top-level)))
                                             (stx-loc top-level)))])
                      #;(printf "Rewritten to ~s~n" (stx->datum (first result)))
                      result)])]
                [else
                 (list top-level)]))
        a-program)))


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


;; get-runtime-source: -> bytes
;; Returns the bytes of all the runtime files as a single chunk.
(define (get-runtime-source)
  (call-with-input-file runtime-manifest-path
    (lambda (ip)
      (apply bytes-append
             (for/list ([line (in-lines ip)])
               (let ([fip (open-input-file (build-path moby-runtime-path line))])
                 (bytes-append (file->bytes (build-path moby-runtime-path line))
                               #"\n")))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; call-with-program/resources: path-string (program/resources -> X) -> X
;; Call f with the program/resources at path f.
(define (call-with-program/resources path f)
  (f (open-program/resources path)))




;; copy-path-to-port: path output-port -> void
(define (copy-path-to-port path outp)
  (call-with-input-file path
    (lambda (ip)
      (copy-port ip outp))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(write-runtime-toplevel-bindings-descriptions)
(write-runtime-library-modules)
(write-compressed-runtime)
#;(write-compiler)
#;(write-compressed-compilers)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#;(provide/contract
   [write-compiler (-> any)]
   [write-compressed-runtime (-> any)])