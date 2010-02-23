#lang scheme/base

(require (only-in scheme/list empty? empty first rest)
         scheme/runtime-path
         scheme/path
         scheme/port
         scheme/file
         scheme/local
         (only-in scheme/list second)
         "compile-helpers.ss"
         "compile-helpers-with-images.ss"
         "program-resources.ss"
         "collects/moby/runtime/stx.ss"
         "collects/moby/runtime/binding.ss"
         "compiler/beginner-to-javascript.ss"
         "compiler/desugar.ss"
         "compiler/analyzer.ss"
         "compiler/helpers.ss"
         "compiler/pinfo.ss"
         "compiler/modules.ss")

(require (for-syntax (only-in scheme/base build-path #%app)))

;; Bootstrap the runtime components of Moby, as well as the
;; Scheme->Javascript compiler in support/js/compiler.js.
;; 
;;
;; * For each library, concatenates all the required modules into a single file.
;;
;; * Compiles the javascript compiler with the javascript compiler.



;; A module record bundles together the name and path of a module.
(define-struct module-record (name path))


(define COLLECTS/MOBY/RUNTIME (build-path "collects" "moby" "runtime"))
(define COLLECTS/BOOTSTRAP (build-path "collects" "bootstrap"))


;; Here is the list of runtime modules.
;; FIXME: compute this, don't hardcode the list.
;; FIXME: we need to do topological sort based on require dependencies.
(define RUNTIME-MODULES
  (list (make-module-record 'moby/runtime/runtime-modules
                            (build-path COLLECTS/MOBY/RUNTIME "runtime-modules.ss"))
        (make-module-record 'moby/runtime/stx 
                            (build-path COLLECTS/MOBY/RUNTIME "stx.ss"))
        (make-module-record 'moby/runtime/binding
                            (build-path COLLECTS/MOBY/RUNTIME "binding.ss"))
        (make-module-record 'moby/runtime/permission-struct
                            (build-path COLLECTS/MOBY/RUNTIME "permission-struct.ss"))
        (make-module-record 'moby/runtime/effect-struct 
                            (build-path COLLECTS/MOBY/RUNTIME "effect-struct.ss"))
        (make-module-record 'moby/runtime/arity-struct 
                            (build-path COLLECTS/MOBY/RUNTIME "arity-struct.ss"))
        (make-module-record 'moby/runtime/error-struct 
                            (build-path COLLECTS/MOBY/RUNTIME "error-struct.ss"))
        (make-module-record 'moby/runtime/scheme-value-to-dom 
                            (build-path COLLECTS/MOBY/RUNTIME "scheme-value-to-dom.ss"))
        (make-module-record 'moby/runtime/dom-helpers
                            (build-path COLLECTS/MOBY/RUNTIME "dom-helpers.ss"))
        (make-module-record 'moby/runtime/dom-parameters
                            (build-path COLLECTS/MOBY/RUNTIME "dom-parameters.ss"))
        (make-module-record 'moby/runtime/error-struct-to-dom 
                            (build-path COLLECTS/MOBY/RUNTIME "error-struct-to-dom.ss"))        
        (make-module-record 'bootstrap/bootstrap-teachpack 
                            (build-path COLLECTS/BOOTSTRAP "bootstrap-teachpack.ss"))
        (make-module-record 'bootstrap/cage-teachpack 
                            (build-path COLLECTS/BOOTSTRAP "cage-teachpack.ss"))
        (make-module-record 'bootstrap/function-teachpack 
                            (build-path COLLECTS/BOOTSTRAP "function-teachpack.ss"))))



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
(define-runtime-path whole-runtime.js "../support/js/runtime/whole-runtime.js")


;; write-compressed-runtime: -> void
;; Write out a runtime of all of the files in the MANIFEST, compressed by the YUI compressor.
(define (write-compressed-runtime)
  (let* ([runtime-source (get-runtime-source)]
         [compressed-runtime-source (compress-and-optimize-source runtime-source)])
    
    (call-with-output-file whole-runtime.js
      (lambda (op) (write-bytes runtime-source op))
      #:exists 'replace)
    
    (call-with-output-file compressed-runtime.js
      (lambda (op) (write-bytes compressed-runtime-source op))
      #:exists 'replace)
    )
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
  (printf "Warning: if collects/moby/runtime/runtime-modules.ss does get changed, you may see an exception during the bootstrap.  You will~n")
  (printf "need to run the bootstrapper one more time to use the refreshed bindings.~n")
  (let ([moby-runtime-module-bindings-description
         `(define MOBY-RUNTIME-MODULE-BINDINGS
            (list ,@
                  (for/list ([a-runtime-module (in-list RUNTIME-MODULES)])
                    ;; FIXME: we need to topologically sort the runtime modules and rerun,
                    ;; because some modules expect others to exist up.
                    ;; (printf "looking at ~s~n" (module-record-path a-runtime-module))
                    (let* ([a-program+resources
                            (open-program/resources (module-record-path a-runtime-module))]
                           [desugared-program+pinfo 
                            (desugar-program (program/resources-program a-program+resources)
                                             (pinfo-update-current-module-path
                                              (get-base-pinfo 'base)
                                              (path->string
                                               (find-relative-path 
                                                (normalize-path "collects")
                                                (normalize-path
                                                 (path->string (module-record-path a-runtime-module)))))))]
                           
                           [a-pinfo (program-analyze/pinfo (first desugared-program+pinfo)
                                                           (second desugared-program+pinfo))])
                      (list 'quote
                            (list
                             (module-record-name a-runtime-module)
                             (path->string (find-relative-path (normalize-path
                                                                  "collects")
                                                                 (normalize-path 
                                                                  (module-record-path a-runtime-module))))
                             (map (lambda (a-binding) 
                                    (binding->sexp 
                                     (localize-binding-to-module 
                                      a-binding 
                                      (module-record-name a-runtime-module))))
                                  (pinfo-get-exposed-bindings a-pinfo))))))))])
    (make-directory* COLLECTS/MOBY/RUNTIME)
    ;; POINT:
    ;; If we screw up computing the module-bindings-description, at least we won't get to this
    ;; point and overwrite runtime-modules.ss, since an exception will have happened up front.
    (call-with-output-file "collects/moby/runtime/runtime-modules.ss"
      (lambda (op)
        (fprintf op "#lang s-exp \"../../../private/restricted-runtime-scheme.ss\"\n")
        (fprintf op ";; This file is automagically generated and maintained by bootstrap-js-compiler.\n;; (in write-runtime-toplevel-bindings-descriptions)\n;; Do not edit this file by hand.\n")
        (write moby-runtime-module-bindings-description op)
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


;; boot-compile-runtime-library: module-name path path -> void
;; Write out the bootstrap-compilation of a Scheme program to a Javascript program.
(define (boot-compile-runtime-library a-module-name a-program-path an-output-path)
  (unless (directory-exists? (path-only an-output-path))
    (make-directory* (path-only an-output-path)))
  (printf "Writing out to ~s~n" an-output-path)
  (call-with-output-file an-output-path
    (lambda (op)
      (display "// This is automatically generated by bootstrap-js-compiler.ss\n" op)
      (display "// Please don't hand-edit this file.\n" op)
      (display (phase-1-bootstrap-compile a-module-name a-program-path)
               op))
    #:exists 'replace))


;; phase-1-bootstrap-compile: path-string -> string
;; The first phase of the bootstrapping compiles programs by treating require statements
;; as literal textual inclusion.
(define (phase-1-bootstrap-compile a-module-name a-path-string)
  (let* ([pinfo-without-debugging-location-emits
          (pinfo-update-with-location-emits? (get-base-pinfo 'moby #;'base)
                                             #f)]
         [path-resolver 
          (pinfo-module-path-resolver pinfo-without-debugging-location-emits)])
    (compiled-program-main/expose-as-module
     (program->compiled-program/pinfo (get-big-program a-path-string pinfo-without-debugging-location-emits)
                                      (pinfo-update-module-path-resolver
                                       
                                       (pinfo-update-current-module-path
                                        pinfo-without-debugging-location-emits
                                        (let ([result
                                               (path->string
                                                (find-relative-path
                                                 (normalize-path "collects")
                                                 (normalize-path a-path-string)))])
                                          (printf "path is: ~s~n" result)
                                          result))
                                       (lambda (path parent-path)
                                         (printf "Module path resolver.  path is ~s, parent ~s~n" path parent-path)
                                         (let ([retargeted-path 
                                                (path->string
                                                 (find-relative-path (normalize-path "collects")
                                                                     (normalize-path 
                                                                      (module-path-join 
                                                                       a-path-string
                                                                       path))))])
                                           
                                           (printf "Retargeted to ~s~n" retargeted-path)
                                           (path-resolver retargeted-path "")))))
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
      (copy-path-to-port "../support/js/runtime/collects/moby/runtime/stx.js" op)
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
       var _permissionStruct = plt.Kernel.invokeModule('moby/runtime/permission-struct');
       var _compiler = plt.Kernel.invokeModule('moby/compiler');

       var exprs = plt.reader.readSchemeExpressions(s, 'standalone');
       var compiledProgram =
           _compiler.EXPORTS['program->compiled-program/pinfo'](exprs, aPinfo);

       var compiledSrc = _compiler.EXPORTS['compiled-program-main'](compiledProgram);
       var permList = _compiler.EXPORTS['pinfo-permissions'](_compiler.EXPORTS['compiled-program-pinfo'](compiledProgram));
       var perms = [];
       while (!permList.isEmpty()) {     
           perms.push(_permissionStruct.EXPORTS['permission->string'](permList.first()));
           permList = permList.rest();
       }
       return [compiledSrc, perms];
   }
   this['compileScheme'] = compileScheme;
   "
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
;; Returns true if the module needs inclusion.
;; WARNING: The path should be relative to collects.
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
                        (module-needs-inclusion? (path->string (find-relative-path 
                                                                (normalize-path "collects")
                                                                (normalize-path 
                                                                 (module-path-join a-path a-subpath))))
                                                 a-pinfo))
                      (get-require-paths (read-program/forget-resources a-path)
                                         (path-only a-path)))])
         (cond
           [(empty? new-paths)
            (list a-path)]
           [else
            (append
             (apply append
                    (map (lambda (a-subpath) 
                           (loop (path->string (build-path (path-only a-path) a-subpath))))
                         new-paths))
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
;; The produced paths are relative to the base path.
(define (get-require-paths a-program base-path)
  (cond
    [(empty? a-program)
     empty]
    [(library-require? (first a-program))
     (append (map (lambda (x)
                    (let ([a-path (stx-e x)])
                      a-path))
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
                   [(module-needs-inclusion? (path->string
                                              (find-relative-path (normalize-path "collects")
                                                                 (normalize-path (module-path-join 
                                                                                  parent-path
                                                                                  (second (stx->datum top-level))))))
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
(write-compiler)
(write-compressed-compilers)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#;(provide/contract
   [write-compiler (-> any)]
   [write-compressed-runtime (-> any)])