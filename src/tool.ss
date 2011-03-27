#lang scheme/base
(require scheme/class
         drscheme/tool
         scheme/gui/base
         framework/framework
         scheme/unit
	 "preferences.ss")

;; Moby hooks.
(provide tool@)



(define-unit tool@
  (import drscheme:tool^)
  (export drscheme:tool-exports^)
    
  (define (phase1) 
    ;; Add a configuration for moby:remote-apk-builder.
    (preferences:add-panel "Moby"
                           (lambda (parent-panel)
                             (define this-panel (new vertical-panel% [parent parent-panel]))
    
                             (define text-field
                               (new text-field% [label "Remote APK builder URL"]
                                    [parent this-panel] 
                                    [init-value 
                                     (get-remote-apk-builder)]))

                             (preferences:add-on-close-dialog-callback
                              (lambda ()
                                (set-remote-apk-builder! (send text-field get-value))))

                             this-panel)))
  

  (define (phase2)
    (void)))