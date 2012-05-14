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


        ;; Bootstrap 2011
	(make-collection-reference
	 'bootstrap2011/bootstrap-teachpack
	 (build-path self "bootstrap2011" "bootstrap-teachpack.ss"))
	(make-collection-reference
	 'bootstrap2011/cage-teachpack
	 (build-path self "bootstrap2011" "cage-teachpack.ss"))
	(make-collection-reference
	 'bootstrap2011/function-teachpack
	 (build-path self "bootstrap2011" "function-teachpack.ss"))
	(make-collection-reference
	 'bootstrap2011/bootstrap-common
	 (build-path self "bootstrap2011" "bootstrap-common.ss"))



        ;; Bootstrap 2012
        (make-collection-reference
	 'bootstrap2012/bootstrap-teachpack
	 (build-path self "bootstrap2012" "bootstrap-teachpack.ss"))
        (make-collection-reference
	 'bootstrap2012/bootstrap-tilt-teachpack
	 (build-path self "bootstrap2012" "bootstrap-tilt-teachpack.ss"))
	(make-collection-reference
	 'bootstrap2012/cage-teachpack
	 (build-path self "bootstrap2012" "cage-teachpack.ss"))
	(make-collection-reference
	 'bootstrap2012/function-teachpack
	 (build-path self "bootstrap2012" "function-teachpack.ss"))
	(make-collection-reference
	 'bootstrap2012/bootstrap-common
	 (build-path self "bootstrap2012" "bootstrap-common.ss"))))


(provide/contract [struct collection-reference ([name module-name?]
                                                [path path?])]
                  
                  [known-collections (listof collection-reference?)])
