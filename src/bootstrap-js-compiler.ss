#lang scheme/base

(require (only-in scheme/list empty? empty first rest)
         scheme/runtime-path
         scheme/path
         scheme/port
         scheme/file
         scheme/contract
         scheme/pretty
         scheme/local
         scheme/bool
         (only-in scheme/list second)
         "compile-helpers.ss"
         "program-resources.ss"
         "runtime/stx.ss"
         "runtime/binding.ss"
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
(define RUNTIME-SRC-PATH "runtime")


;; Here's the list of runtime modules.
(define RUNTIME-MODULES
  (list (make-module-record 'moby/runtime/stx (build-path RUNTIME-SRC-PATH "stx.ss"))
        (make-module-record 'moby/runtime/binding (build-path RUNTIME-SRC-PATH "binding.ss"))
        (make-module-record 'moby/runtime/permission-struct (build-path RUNTIME-SRC-PATH "permission-struct.ss"))
        (make-module-record 'moby/runtime/effect-struct (build-path RUNTIME-SRC-PATH "effect-struct.ss"))
        (make-module-record 'moby/runtime/arity-struct (build-path RUNTIME-SRC-PATH "arity-struct.ss"))
        (make-module-record 'moby/runtime/error-struct (build-path RUNTIME-SRC-PATH "error-struct.ss"))))



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
;; Also writes a compressed version of the standalone compiler.
(define (write-compressed-runtime)
  (write-compiler)

  (let* ([runtime-source (get-runtime-source)]
         [compressed-runtime-source (compress-and-optimize-source runtime-source)])
    (call-with-output-file compressed-runtime.js
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




;; write-collection-module: -> void
;; Writes out the optional teachpack modules
(define (write-collection-modules)
  (write-collect-module "bootstrap-teachpack.js" "collects/bootstrap-teachpack.ss")
  (write-collect-module "cage-teachpack.js" "collects/cage-teachpack.ss")
  (write-collect-module "function-teachpack.js" "collects/function-teachpack.ss"))


;; write-runtime-toplevel-bindings-descriptions: -> void
;; Write out compiler/gen/runtime-modules.ss, which describes the module binding
;; for the runtime modules listed in RUNTIME-MODULE-PATHS.

;; FIXME: we need to export permission information here too!
(define (write-runtime-toplevel-bindings-descriptions)
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
                                    (binding->sexp a-binding))
                                  (pinfo-get-exposed-bindings a-pinfo))))))))])
    ;; POINT:
    ;; If we screw up computing the module-bindings-description, at least we won't get to this
    ;; point and overwrite runtime-modules.ss, since an exception will have happened up front.
    (call-with-output-file "compiler/gen/runtime-modules.ss"
      (lambda (op)
        (fprintf op "#lang s-exp \"../lang.ss\"\n")
        (fprintf op ";; This file is automagically generated and maintained by bootstrap-js-compiler.\n;; (in write-runtime-toplevel-bindings-descriptions)\n;; Do not edit this file by hand.\n")
        (pretty-print moby-runtime-module-bindings-description op)
        (newline op)
        (display '(provide MOBY-RUNTIME-MODULE-BINDINGS) op))
      
      #:exists 'replace)))





;; write-collect-module: string path-string -> void
;; Write out the content of the module into the collects path.
(define (write-collect-module module-name src-path)
  (unless (directory-exists? (build-path moby-runtime-path "collects"))
    (make-directory (build-path moby-runtime-path "collects")))
                             
  (call-with-output-file (build-path moby-runtime-path "collects" module-name)
    (lambda (op)
      (display (compiled-program-main/expose-as-module
                (program->compiled-program/pinfo (read-program/forget-resources src-path)
                                                 (pinfo-update-current-module-path
                                                  (get-base-pinfo 'moby)
                                                  src-path))
                module-name)
               op))
    #:exists 'replace))




;; write-compiler: ->void
;; Writes out the javascript compiler and other files.
;; Generates: compiler.js, standalone-compiler.js, permission-struct.js
(define (write-compiler)
  (boot-compile-runtime-library "compiler/beginner-to-javascript.ss" compiler-path)

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
      (display (phase-1-bootstrap-compile "compiler/beginner-to-javascript.ss") op)
      
      
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
      



;; write-runtime-library-modules: -> void
;; Write out the runtime library.
(define (write-runtime-library-modules)
  (local [(define (get-js-target a-path-string)
            (string-append
             (substring (string-append "../support/js/" a-path-string)
                        0
                        (- (string-length 
                            (string-append "../support/js/" a-path-string))
                           3))
             ".js"))]
  (for ([a-runtime-module (in-list RUNTIME-MODULES)])
    (boot-compile-runtime-library (path->string (module-record-path a-runtime-module))
                                  (get-js-target (path->string (module-record-path a-runtime-module)))))))


;; boot-compile-runtime-library: path path -> void
;; Write out the bootstrap-compilation of a Scheme program to a Javascript program.
(define (boot-compile-runtime-library a-program-path an-output-path)
  (call-with-output-file an-output-path
    (lambda (op)
      (display "// This is automatically generated by bootstrap-js-compiler.ss\n" op)
      (display "// Please don't hand-edit this file.\n" op)
      (display (phase-1-bootstrap-compile a-program-path)
             op))
    #:exists 'replace))



;; phase-1-bootstrap-compile: path-string -> string
;; The first phase of the bootstrapping compiles programs by treating require statements
;; as literal textual inclusion.
(define (phase-1-bootstrap-compile a-path-string)
  (let ([pinfo-without-debugging-location-emits
         (pinfo-update-with-location-emits? (get-base-pinfo 'base)
                                            #f)])
    (compiled-program-main/expose
     (program->compiled-program/pinfo (get-big-program a-path-string pinfo-without-debugging-location-emits)
                                      (pinfo-update-current-module-path
                                       pinfo-without-debugging-location-emits
                                       a-path-string)))))


;; get-big-program: path pinfo -> program
(define (get-big-program a-path a-pinfo)
  (let* ([modules (find-transitive-required-modules a-path a-pinfo)]
         [big-program (apply append (map (lambda (p)
                                           (remove-requires
                                            (read-program/forget-resources p)
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
(define (remove-requires a-program a-path a-pinfo)
  (filter (lambda (top-level)
            (cond [(stx-begins-with? top-level 'require)
                   (cond
                     [(module-needs-inclusion? a-path a-pinfo)
                      false]
                     [else
                      true])]
                  [else
                   true]))
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
(write-collection-modules)
(write-compressed-runtime)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide/contract
 [write-compiler (-> any)]
 [write-compressed-runtime (-> any)])