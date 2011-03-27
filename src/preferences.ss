#lang scheme/base

(require framework/preferences)

(provide get-remote-apk-builder
         set-remote-apk-builder!)

(preferences:set-default 'moby:remote-apk-builder 
                         "http://go.cs.brown.edu/"
                         string?)


(define (get-remote-apk-builder)
  (preferences:get 'moby:remote-apk-builder))

(define (set-remote-apk-builder! v)
  (preferences:set 'moby:remote-apk-builder v))