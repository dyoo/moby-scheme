#lang scheme/base
(require scheme/contract
         scheme/file)

(provide/contract [copy-directory/files* (path-string? path-string? . -> . any)]
                  [upper-camel-case (string? . -> . string?)])

;; copy-directory/files*: path path -> void
;; Like copy-directory/files, but overwrites rather than raises exn:fail:filesystem.
(define (copy-directory/files* from-path dest-path)
  (for ([file (directory-list from-path)])
    (when (file-exists? (build-path dest-path file))
      (delete-file (build-path dest-path file)))
    (when (directory-exists? (build-path dest-path file))
      (delete-directory/files (build-path dest-path file)))
    (copy-directory/files (build-path from-path file)
                          (build-path dest-path file))))





;; upper-camel-case: string -> string
;; Given a string name, perform an UpperCamelCasing of the name.
(define (upper-camel-case name)
  (apply string-append
         (map string-titlecase (regexp-split #px"\\s+" 
                                             (regexp-replace* #px"[^\\s\\w]+" name "")))))