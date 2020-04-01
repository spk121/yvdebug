;; This module manages the about popup window

(define-module (yvdebug about)
  #:use-module (yvdebug typelib)
  #:use-module (oop goops)
  #:use-module (gi)
  #:export (do-about))

(define (do-about window)
  (let ((about (about-dialog:new)))
    (set-program-name about "YVDebug")
    (set-version about "0.0")
    (set-copyright about "Copyright 2020 Michael Gran")
    (set-license-type about (symbol->license 'gpl-3-0))
    
    (set-transient-for about window)
    (show-all about)))
