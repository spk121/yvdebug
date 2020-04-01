;; This module manages the shortcuts popup window

(define-module (yvdebug shortcuts)
  #:use-module (yvdebug typelib)
  #:use-module (oop goops)
  #:use-module (gi)
  #:export (do-shortcuts))

(define (do-shortcuts window)
   (let ((builder (builder:new-from-resource "/com/lonelycactus/yvdebug/shortcuts.ui")))
     (define (obj str)
       (get-object builder str))
     (let ((overlay (obj "shortcuts-window")))
       ;; (set! (section-name overlay) "shortcuts")
       (set-transient-for overlay window)
       (show-all overlay)
       )))
