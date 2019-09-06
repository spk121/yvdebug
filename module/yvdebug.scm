(define-module (yvdebug)
  #:use-module (ice-9 command-line)
  #:use-module (ice-9 binary-ports)
  #:use-module (yvdebug terminal)
  #:use-module (system repl repl)
  #:use-module (yvdebug typelib)
  #:export (main))

(define (activate app)
  (let ((appwindow (application-window:new app))
        (terminal (make-terminal (terminal:new))))
    (add appwindow (get-vte-terminal terminal))
    (attach-current-io-ports terminal)
    (show-all appwindow)
    (call-with-new-thread
     (lambda ()
       (start-repl #:debug #t)))
    #t))

(define (main args)
  (let ((app (application:new "com.lonelycactus.yvdebug"
                              (number->application-flags 0))))
    (connect app application:activate activate)
    
    ;; Don't pass command line here: We don't want any of the command
    ;; line being interpreted by Gtk.
    (application:run app)))
