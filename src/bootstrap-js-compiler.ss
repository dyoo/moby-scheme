#lang scheme/base

(require (only-in scheme/list empty? empty first rest)
         scheme/runtime-path
         scheme/path
         scheme/port
         scheme/file
         scheme/contract
         (only-in scheme/list second)
         "runtime/stx.ss"
         "compiler/pinfo.ss"
         "compile-helpers.ss"
         "program-resources.ss"
         "compiler/beginner-to-javascript.ss"
         "compiler/desugar.ss"
         "compiler/analyzer.ss"
         "compiler/helpers.ss")

(require (for-syntax (only-in scheme/base build-path)))

;; Bootstrap the runtime components of Moby, as well as the
;; Scheme->Javascript compiler in support/js/compiler.js.
;; 
;; * Translates provide/contracts into provides  (FIXME: doesn't preserve the contracts yet)
;;
;; * For each library, concatenates all the required modules into a single file.
;;
;; * Compiles the javascript compiler with the javascript compiler.




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
(define (write-runtime-toplevel-bindings-descriptions)
  (for ([a-path (in-list 
                 (list "runtime/stx.ss"
                       "runtime/permission-struct.ss"
                       "runtime/effect-struct.ss"
                       "runtime/arity-struct.ss"
                       "runtime/error-struct.ss"))])
    (let* ([a-program+resources
            (open-program/resources a-path)]
           [desugared-program+pinfo 
            (desugar-program (program/resources-program a-program+resources)
                             (get-base-pinfo 'base))]
          
           [a-pinfo (program-analyze/pinfo (first desugared-program+pinfo)
                                           (second desugared-program+pinfo))])
      (void))))




;; write-collect-module: string path-string -> void
;; Write out the content of the module into the collects path.
(define (write-collect-module module-name src-path)
  (unless (directory-exists? (build-path moby-runtime-path "collects"))
    (make-directory (build-path moby-runtime-path "collects")))
                             
  (call-with-output-file (build-path moby-runtime-path "collects" module-name)
    (lambda (op)
      (display (compiled-program-main/expose-as-module
                (program->compiled-program/pinfo (read-program/forget-resources src-path)
                                                 (get-base-pinfo 'moby))
                module-name)
               op))
    #:exists 'replace))




;; write-compiler: ->void
;; Writes out the javascript compiler and other files.
;; Generates: compiler.js, standalone-compiler.js, permission-struct.js
(define (write-compiler)
  (write-runtime-library-modules)
  
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
  (boot-compile-runtime-library "runtime/stx.ss" "../support/js/runtime/stx.js")
  (boot-compile-runtime-library "runtime/permission-struct.ss" "../support/js/runtime/permission-struct.js")
  (boot-compile-runtime-library "runtime/effect-struct.ss" "../support/js/runtime/effect-struct.js")
  (boot-compile-runtime-library "runtime/arity-struct.ss"  "../support/js/runtime/arity-struct.js")
  (boot-compile-runtime-library "runtime/error-struct.ss"  "../support/js/runtime/error-struct.js"))


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



;; copy-path-to-port: path output-port -> void
(define (copy-path-to-port path outp)
  (call-with-input-file path
    (lambda (ip)
      (copy-port ip outp))))




;; phase-1-bootstrap-compile: path -> string
;; The first phase of the bootstrapping compiles programs by treating require statements
;; as literal textual inclusion.
(define (phase-1-bootstrap-compile a-path)
  (let ([pinfo-without-debugging-location-emits
         (pinfo-update-with-location-emits? (get-base-pinfo 'base)
                                            #f)])
    (compiled-program-main/expose
     (program->compiled-program/pinfo (get-big-program a-path)
                                      pinfo-without-debugging-location-emits))))


;; get-big-program: path -> program
(define (get-big-program a-path)
  (let* ([modules (find-transitive-required-modules a-path)]
         [big-program (apply append (map (lambda (p)
                                           (remove-requires
                                            (read-program/forget-resources p)))
                                         modules))])
    big-program))
  



;; find-transitive-required-modules: path -> (listof path)
(define (find-transitive-required-modules a-path)
  (unique
   (let loop ([a-path a-path])
     (let ([new-paths 
            (get-require-paths (read-program/forget-resources a-path)
                               (path-only a-path))])
       (cond
         [(empty? new-paths)
          (list a-path)]
         [else
          (append
           (apply append
                  (map loop new-paths))
           (list a-path))])))))
     


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



;; convert-provide/contract-clause: stx -> stx
(define (convert-provide/contract-clause a-clause)
  (cond
    [(stx-begins-with? a-clause 'struct)
     (datum->stx `(struct-out ,(first (rest (stx-e a-clause))))
                 (stx-loc a-clause))]
    [(list? (stx-e a-clause))
     (first (stx-e a-clause))]
    [(symbol? (stx-e a-clause))
     a-clause]))




;; remove-requires: program -> program
(define (remove-requires a-program)
  (filter (lambda (top-level)
            (not (stx-begins-with? top-level 'require)))
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


;; call-with-program/resources: path-string (program/resources -> X) -> X
;; Call f with the program/resources at path f.
(define (call-with-program/resources path f)
  (f (open-program/resources path)))



(write-runtime-toplevel-bindings-descriptions)
(write-collection-modules)
(write-compressed-runtime)



(provide/contract
 [write-compiler (-> any)]
 [write-compressed-runtime (-> any)])