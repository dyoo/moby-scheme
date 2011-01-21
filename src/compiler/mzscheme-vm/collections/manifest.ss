#lang scheme/base
(require scheme/contract
         scheme/runtime-path
         "../../../collects/moby/runtime/binding.ss")

;; hardcoded manifest
(define-runtime-path self ".")

(define-struct collection-reference (name path))

(define known-collections
  (list (make-collection-reference 
         'bootstrap/bootstrap-teachpack 
         (build-path self "bootstrap" "bootstrap-teachpack-translated.ss"))
        
        (make-collection-reference 
         'bootstrap/function-teachpack
         (build-path self "bootstrap" "function-teachpack-translated.ss"))
        
        (make-collection-reference
         'bootstrap/cage-teachpack
         (build-path self "bootstrap" "cage-teachpack-translated.ss"))
      
	(make-collection-reference
	 'bootstrap/bootstrap-gtp-teachpack
	 (build-path self "bootstrap" "bootstrap-gtp-teachpack.ss"))
  
	(make-collection-reference
         'bootstrap/autos-teachpack
         (build-path self "bootstrap" "autos.ss"))

        (make-collection-reference 
         'jsworld/google-maps 
         (build-path self "jsworld" "google-maps.ss"))

        (make-collection-reference 
         'jsworld/phonegap 
         (build-path self "jsworld" "phonegap.ss"))


	(make-collection-reference
	 'bootstrap2011/bootstrap-teachpack
	 (build-path self "bootstrap2011" "bootstrap-teachpack.ss"))


))


(provide/contract [struct collection-reference ([name module-name?]
                                                [path path?])]
                  
                  [known-collections (listof collection-reference?)])
